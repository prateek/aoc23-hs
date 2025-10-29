//
//  StoryViewModel.swift
//  StoryCreator
//
//  ViewModel for story creation and generation
//

import Foundation
import Combine

@MainActor
class StoryViewModel: ObservableObject {
    @Published var story: Story
    @Published var characters: [Character] = []
    @Published var isGenerating = false
    @Published var generationProgress: Double = 0.0
    @Published var errorMessage: String?

    private let llmService = LLMService.shared
    private let elevenLabsService = ElevenLabsService.shared
    private let yotoService = YotoService.shared

    init(story: Story = Story()) {
        self.story = story
    }

    // MARK: - Story Generation

    func generateCompleteStory() async {
        isGenerating = true
        generationProgress = 0.0
        errorMessage = nil

        do {
            // Step 1: Generate scene text
            generationProgress = 0.1
            for (index, scene) in story.scenes.enumerated() {
                let sceneText = try await llmService.generateSceneText(
                    scene: scene,
                    story: story,
                    characters: characters
                )
                story.scenes[index].generatedText = sceneText
                generationProgress = 0.1 + (0.3 * Double(index + 1) / Double(story.scenes.count))
            }

            // Step 2: Generate full story text
            generationProgress = 0.5
            let fullStory = try await llmService.generateStoryFromScenes(
                story: story,
                characters: characters,
                scenes: story.scenes
            )
            story.generatedText = fullStory

            // Step 3: Generate audio for each scene
            generationProgress = 0.6
            let audioTracks = try await elevenLabsService.generateAudioForStory(
                story: story,
                scenes: story.scenes,
                characters: characters
            )
            story.audioTracks = audioTracks

            generationProgress = 1.0
            story.updatedAt = Date()
        } catch {
            errorMessage = error.localizedDescription
        }

        isGenerating = false
    }

    func generateSceneText(for sceneIndex: Int) async {
        guard sceneIndex < story.scenes.count else { return }

        isGenerating = true
        errorMessage = nil

        do {
            let scene = story.scenes[sceneIndex]
            let sceneText = try await llmService.generateSceneText(
                scene: scene,
                story: story,
                characters: characters
            )
            story.scenes[sceneIndex].generatedText = sceneText
            story.updatedAt = Date()
        } catch {
            errorMessage = error.localizedDescription
        }

        isGenerating = false
    }

    // MARK: - Audio Generation

    func generateAudioForScene(at index: Int) async {
        guard index < story.scenes.count else { return }

        let scene = story.scenes[index]
        guard let sceneText = scene.generatedText, !sceneText.isEmpty else {
            errorMessage = "Please generate scene text first"
            return
        }

        isGenerating = true
        errorMessage = nil

        do {
            let voiceId = getVoiceForScene(scene)
            let audioData = try await elevenLabsService.generateAudio(
                text: sceneText,
                voiceId: voiceId
            )

            // Save audio file
            let fileName = "\(story.id.uuidString)_\(scene.id.uuidString).mp3"
            let documentsPath = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
            let audioDirectory = documentsPath.appendingPathComponent("AudioTracks", isDirectory: true)

            if !FileManager.default.fileExists(atPath: audioDirectory.path) {
                try FileManager.default.createDirectory(at: audioDirectory, withIntermediateDirectories: true)
            }

            let fileURL = audioDirectory.appendingPathComponent(fileName)
            try audioData.write(to: fileURL)

            let audioTrack = AudioTrack(
                title: scene.title,
                sceneId: scene.id,
                text: sceneText,
                voiceId: voiceId,
                voiceName: "Default Voice",
                localAudioPath: fileURL.path,
                fileSize: Int64(audioData.count),
                generationStatus: .completed
            )

            if let existingIndex = story.audioTracks.firstIndex(where: { $0.sceneId == scene.id }) {
                story.audioTracks[existingIndex] = audioTrack
            } else {
                story.audioTracks.append(audioTrack)
            }

            story.updatedAt = Date()
        } catch {
            errorMessage = error.localizedDescription
        }

        isGenerating = false
    }

    // MARK: - Yoto Integration

    func uploadToYoto() async {
        guard !story.audioTracks.isEmpty else {
            errorMessage = "No audio tracks to upload"
            return
        }

        isGenerating = true
        errorMessage = nil

        do {
            let yotoPlaylist = try await yotoService.uploadStoryToYoto(
                story: story,
                audioTracks: story.audioTracks
            )
            story.yotoPlaylistId = yotoPlaylist.id
            story.updatedAt = Date()
        } catch {
            errorMessage = error.localizedDescription
        }

        isGenerating = false
    }

    // MARK: - Helper Methods

    private func getVoiceForScene(_ scene: StoryScene) -> String {
        for characterId in scene.characterIds {
            if let character = characters.first(where: { $0.id == characterId }),
               let voiceId = character.elevenLabsVoiceId {
                return voiceId
            }
        }
        return "21m00Tcm4TlvDq8ikWAM" // Default voice
    }

    func addScene() {
        let newScene = StoryScene(orderIndex: story.scenes.count)
        story.scenes.append(newScene)
        story.updatedAt = Date()
    }

    func removeScene(at index: Int) {
        guard index < story.scenes.count else { return }
        story.scenes.remove(at: index)
        // Reorder remaining scenes
        for (idx, _) in story.scenes.enumerated() {
            story.scenes[idx].orderIndex = idx
        }
        story.updatedAt = Date()
    }

    func moveScene(from source: IndexSet, to destination: Int) {
        story.scenes.move(fromOffsets: source, toOffset: destination)
        // Reorder scenes
        for (idx, _) in story.scenes.enumerated() {
            story.scenes[idx].orderIndex = idx
        }
        story.updatedAt = Date()
    }

    func loadCharacters(_ chars: [Character]) {
        self.characters = chars
    }
}
