//
//  ElevenLabsService.swift
//  StoryCreator
//
//  Service for ElevenLabs audio generation
//

import Foundation
import AVFoundation

class ElevenLabsService {
    static let shared = ElevenLabsService()
    private let config = APIConfiguration.shared

    private init() {}

    // MARK: - Voice Management

    func fetchAvailableVoices() async throws -> [ElevenLabsVoice] {
        guard !config.elevenLabsKey.isEmpty else {
            throw ElevenLabsError.missingAPIKey
        }

        let url = URL(string: "https://api.elevenlabs.io/v1/voices")!
        var request = URLRequest(url: url)
        request.addValue(config.elevenLabsKey, forHTTPHeaderField: "xi-api-key")

        let (data, response) = try await URLSession.shared.data(for: request)

        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            throw ElevenLabsError.requestFailed
        }

        let result = try JSONDecoder().decode(VoicesResponse.self, from: data)
        return result.voices
    }

    // MARK: - Audio Generation

    func generateAudio(
        text: String,
        voiceId: String,
        modelId: String = "eleven_multilingual_v2"
    ) async throws -> Data {
        guard !config.elevenLabsKey.isEmpty else {
            throw ElevenLabsError.missingAPIKey
        }

        let url = URL(string: "https://api.elevenlabs.io/v1/text-to-speech/\(voiceId)")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.addValue(config.elevenLabsKey, forHTTPHeaderField: "xi-api-key")
        request.addValue("application/json", forHTTPHeaderField: "Content-Type")

        let body: [String: Any] = [
            "text": text,
            "model_id": modelId,
            "voice_settings": [
                "stability": 0.5,
                "similarity_boost": 0.75,
                "style": 0.5,
                "use_speaker_boost": true
            ]
        ]

        request.httpBody = try JSONSerialization.data(withJSONObject: body)

        let (data, response) = try await URLSession.shared.data(for: request)

        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            if let errorString = String(data: data, encoding: .utf8) {
                print("ElevenLabs Error: \(errorString)")
            }
            throw ElevenLabsError.requestFailed
        }

        return data
    }

    func generateAudioForStory(
        story: Story,
        scenes: [StoryScene],
        characters: [Character]
    ) async throws -> [AudioTrack] {
        var audioTracks: [AudioTrack] = []

        for scene in scenes.sorted(by: { $0.orderIndex < $1.orderIndex }) {
            guard let sceneText = scene.generatedText, !sceneText.isEmpty else {
                continue
            }

            // Use first character's voice or default voice
            let voiceId = getVoiceForScene(scene: scene, characters: characters)

            do {
                let audioData = try await generateAudio(text: sceneText, voiceId: voiceId)

                // Save audio to documents directory
                let fileName = "\(story.id.uuidString)_\(scene.id.uuidString).mp3"
                let localPath = try saveAudioToDocuments(audioData: audioData, fileName: fileName)

                // Get audio duration
                let duration = try getAudioDuration(from: localPath)

                let track = AudioTrack(
                    title: scene.title,
                    sceneId: scene.id,
                    text: sceneText,
                    voiceId: voiceId,
                    voiceName: "Default Voice",
                    localAudioPath: localPath,
                    duration: duration,
                    fileSize: Int64(audioData.count),
                    generationStatus: .completed
                )

                audioTracks.append(track)
            } catch {
                let track = AudioTrack(
                    title: scene.title,
                    sceneId: scene.id,
                    text: sceneText,
                    voiceId: voiceId,
                    voiceName: "Default Voice",
                    generationStatus: .failed,
                    errorMessage: error.localizedDescription
                )
                audioTracks.append(track)
            }
        }

        return audioTracks
    }

    // MARK: - Helper Methods

    private func getVoiceForScene(scene: StoryScene, characters: [Character]) -> String {
        // Find first character in scene with a voice ID
        for characterId in scene.characterIds {
            if let character = characters.first(where: { $0.id == characterId }),
               let voiceId = character.elevenLabsVoiceId {
                return voiceId
            }
        }

        // Return default voice (Rachel)
        return "21m00Tcm4TlvDq8ikWAM"
    }

    private func saveAudioToDocuments(audioData: Data, fileName: String) throws -> String {
        let documentsPath = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
        let audioDirectory = documentsPath.appendingPathComponent("AudioTracks", isDirectory: true)

        // Create directory if it doesn't exist
        if !FileManager.default.fileExists(atPath: audioDirectory.path) {
            try FileManager.default.createDirectory(at: audioDirectory, withIntermediateDirectories: true)
        }

        let fileURL = audioDirectory.appendingPathComponent(fileName)
        try audioData.write(to: fileURL)

        return fileURL.path
    }

    private func getAudioDuration(from path: String) throws -> TimeInterval {
        let url = URL(fileURLWithPath: path)
        let audioAsset = AVURLAsset(url: url)
        return CMTimeGetSeconds(audioAsset.duration)
    }

    func deleteAudioFile(at path: String) throws {
        let url = URL(fileURLWithPath: path)
        try FileManager.default.removeItem(at: url)
    }
}

// MARK: - Models

struct VoicesResponse: Codable {
    let voices: [ElevenLabsVoice]
}

struct ElevenLabsVoice: Codable, Identifiable {
    let voiceId: String
    let name: String
    let previewUrl: String?
    let category: String?
    let labels: [String: String]?

    var id: String { voiceId }

    enum CodingKeys: String, CodingKey {
        case voiceId = "voice_id"
        case name
        case previewUrl = "preview_url"
        case category
        case labels
    }
}

// MARK: - Errors

enum ElevenLabsError: LocalizedError {
    case missingAPIKey
    case requestFailed
    case invalidAudioData

    var errorDescription: String? {
        switch self {
        case .missingAPIKey:
            return "ElevenLabs API key is missing. Please configure it in settings."
        case .requestFailed:
            return "Failed to generate audio. Please check your internet connection and API key."
        case .invalidAudioData:
            return "Received invalid audio data."
        }
    }
}
