//
//  LLMService.swift
//  StoryCreator
//
//  Service for LLM text generation (OpenAI/Gemini)
//

import Foundation

class LLMService {
    static let shared = LLMService()
    private let config = APIConfiguration.shared

    private init() {}

    // MARK: - Story Generation

    func generateStoryFromScenes(
        story: Story,
        characters: [Character],
        scenes: [StoryScene]
    ) async throws -> String {
        let prompt = buildStoryPrompt(story: story, characters: characters, scenes: scenes)

        switch config.selectedLLMProvider {
        case .openAI:
            return try await generateWithOpenAI(prompt: prompt, maxTokens: 4000)
        case .gemini:
            return try await generateWithGemini(prompt: prompt, maxTokens: 4000)
        }
    }

    func generateSceneText(
        scene: StoryScene,
        story: Story,
        characters: [Character]
    ) async throws -> String {
        let prompt = buildScenePrompt(scene: scene, story: story, characters: characters)

        switch config.selectedLLMProvider {
        case .openAI:
            return try await generateWithOpenAI(prompt: prompt, maxTokens: 1000)
        case .gemini:
            return try await generateWithGemini(prompt: prompt, maxTokens: 1000)
        }
    }

    func enhanceCharacterDescription(character: Character) async throws -> String {
        let prompt = """
        Enhance this character description for a children's story:
        Name: \(character.name)
        Age: \(character.age)
        Role: \(character.role.rawValue)
        Current Description: \(character.appearance)
        Personality: \(character.personality)

        Provide a vivid, child-friendly description that would help bring this character to life.
        Keep it appropriate for children and engaging.
        """

        switch config.selectedLLMProvider {
        case .openAI:
            return try await generateWithOpenAI(prompt: prompt, maxTokens: 300)
        case .gemini:
            return try await generateWithGemini(prompt: prompt, maxTokens: 300)
        }
    }

    func suggestPlotIdeas(
        genre: StoryGenre,
        ageRange: AgeRange,
        theme: String
    ) async throws -> [String] {
        let prompt = """
        Generate 5 creative plot ideas for a children's story with these parameters:
        Genre: \(genre.rawValue)
        Age Range: \(ageRange.rawValue)
        Theme: \(theme)

        Provide 5 distinct, engaging plot ideas suitable for this age group.
        Format each as a numbered item.
        """

        let response: String
        switch config.selectedLLMProvider {
        case .openAI:
            response = try await generateWithOpenAI(prompt: prompt, maxTokens: 500)
        case .gemini:
            response = try await generateWithGemini(prompt: prompt, maxTokens: 500)
        }

        return response.components(separatedBy: "\n")
            .filter { !$0.isEmpty }
            .map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
    }

    // MARK: - Private Helper Methods

    private func buildStoryPrompt(
        story: Story,
        characters: [Character],
        scenes: [StoryScene]
    ) -> String {
        var prompt = """
        Write a complete children's story with the following details:

        Title: \(story.title)
        Genre: \(story.genre.rawValue)
        Target Age: \(story.targetAge.rawValue)
        Theme: \(story.theme)
        Setting: \(story.setting)
        Mood: \(story.mood)
        Synopsis: \(story.synopsis)

        Characters:
        """

        for character in characters {
            prompt += """

            - \(character.name): \(character.role.rawValue)
              Description: \(character.appearance)
              Personality: \(character.personality)
            """
        }

        prompt += "\n\nStory Structure:\n"
        for (index, scene) in scenes.enumerated() {
            prompt += """

            Scene \(index + 1): \(scene.title)
            - Location: \(scene.location)
            - Description: \(scene.description)
            - Plot Points: \(scene.plotPoints.joined(separator: ", "))
            """
        }

        prompt += """


        Write a complete, engaging story following this structure.
        Make it age-appropriate, entertaining, and well-paced for reading aloud.
        Use clear, simple language and vivid imagery.
        """

        return prompt
    }

    private func buildScenePrompt(
        scene: StoryScene,
        story: Story,
        characters: [Character]
    ) -> String {
        let sceneCharacters = characters.filter { scene.characterIds.contains($0.id) }

        return """
        Write a scene for a children's story:

        Story Context:
        - Genre: \(story.genre.rawValue)
        - Age Range: \(story.targetAge.rawValue)
        - Setting: \(story.setting)

        Scene Details:
        - Title: \(scene.title)
        - Location: \(scene.location)
        - Description: \(scene.description)
        - Plot Points: \(scene.plotPoints.joined(separator: ", "))
        - Emotional Tone: \(scene.emotion)

        Characters in Scene:
        \(sceneCharacters.map { "- \($0.name)" }.joined(separator: "\n"))

        Write this scene in an engaging, age-appropriate way.
        """
    }

    // MARK: - OpenAI Integration

    private func generateWithOpenAI(prompt: String, maxTokens: Int) async throws -> String {
        guard !config.openAIKey.isEmpty else {
            throw LLMError.missingAPIKey
        }

        let url = URL(string: "https://api.openai.com/v1/chat/completions")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.addValue("Bearer \(config.openAIKey)", forHTTPHeaderField: "Authorization")
        request.addValue("application/json", forHTTPHeaderField: "Content-Type")

        let body: [String: Any] = [
            "model": "gpt-4o-mini",
            "messages": [
                ["role": "system", "content": "You are a creative children's story writer."],
                ["role": "user", "content": prompt]
            ],
            "max_tokens": maxTokens,
            "temperature": 0.8
        ]

        request.httpBody = try JSONSerialization.data(withJSONObject: body)

        let (data, response) = try await URLSession.shared.data(for: request)

        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            throw LLMError.requestFailed
        }

        let result = try JSONDecoder().decode(OpenAIResponse.self, from: data)
        guard let content = result.choices.first?.message.content else {
            throw LLMError.invalidResponse
        }

        return content
    }

    // MARK: - Gemini Integration

    private func generateWithGemini(prompt: String, maxTokens: Int) async throws -> String {
        guard !config.geminiKey.isEmpty else {
            throw LLMError.missingAPIKey
        }

        let url = URL(string: "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent?key=\(config.geminiKey)")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.addValue("application/json", forHTTPHeaderField: "Content-Type")

        let body: [String: Any] = [
            "contents": [
                [
                    "parts": [
                        ["text": prompt]
                    ]
                ]
            ],
            "generationConfig": [
                "maxOutputTokens": maxTokens,
                "temperature": 0.8
            ]
        ]

        request.httpBody = try JSONSerialization.data(withJSONObject: body)

        let (data, response) = try await URLSession.shared.data(for: request)

        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            throw LLMError.requestFailed
        }

        let result = try JSONDecoder().decode(GeminiResponse.self, from: data)
        guard let content = result.candidates.first?.content.parts.first?.text else {
            throw LLMError.invalidResponse
        }

        return content
    }
}

// MARK: - Response Models

struct OpenAIResponse: Codable {
    let choices: [OpenAIChoice]
}

struct OpenAIChoice: Codable {
    let message: OpenAIMessage
}

struct OpenAIMessage: Codable {
    let content: String
}

struct GeminiResponse: Codable {
    let candidates: [GeminiCandidate]
}

struct GeminiCandidate: Codable {
    let content: GeminiContent
}

struct GeminiContent: Codable {
    let parts: [GeminiPart]
}

struct GeminiPart: Codable {
    let text: String
}

// MARK: - Errors

enum LLMError: LocalizedError {
    case missingAPIKey
    case requestFailed
    case invalidResponse

    var errorDescription: String? {
        switch self {
        case .missingAPIKey:
            return "API key is missing. Please configure your API keys in settings."
        case .requestFailed:
            return "Failed to generate content. Please check your internet connection and API key."
        case .invalidResponse:
            return "Received invalid response from the API."
        }
    }
}
