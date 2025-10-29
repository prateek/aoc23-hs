//
//  CharacterViewModel.swift
//  StoryCreator
//
//  ViewModel for character management
//

import Foundation
import Combine

@MainActor
class CharacterViewModel: ObservableObject {
    @Published var characters: [Character] = []
    @Published var selectedCharacter: Character?
    @Published var availableVoices: [ElevenLabsVoice] = []
    @Published var isLoading = false
    @Published var errorMessage: String?

    private let llmService = LLMService.shared
    private let elevenLabsService = ElevenLabsService.shared

    // MARK: - Character Management

    func addCharacter(_ character: Character) {
        characters.append(character)
    }

    func updateCharacter(_ character: Character) {
        if let index = characters.firstIndex(where: { $0.id == character.id }) {
            characters[index] = character
        }
    }

    func deleteCharacter(_ character: Character) {
        characters.removeAll { $0.id == character.id }
    }

    func createNewCharacter() -> Character {
        return Character()
    }

    // MARK: - AI Enhancement

    func enhanceCharacterDescription(for character: Character) async {
        isLoading = true
        errorMessage = nil

        do {
            let enhancedDescription = try await llmService.enhanceCharacterDescription(character: character)
            var updatedCharacter = character
            updatedCharacter.appearance = enhancedDescription
            updatedCharacter.updatedAt = Date()
            updateCharacter(updatedCharacter)
        } catch {
            errorMessage = error.localizedDescription
        }

        isLoading = false
    }

    // MARK: - Voice Management

    func fetchAvailableVoices() async {
        isLoading = true
        errorMessage = nil

        do {
            availableVoices = try await elevenLabsService.fetchAvailableVoices()
        } catch {
            errorMessage = error.localizedDescription
        }

        isLoading = false
    }

    func assignVoiceToCharacter(voiceId: String, voiceName: String, character: Character) {
        var updatedCharacter = character
        updatedCharacter.elevenLabsVoiceId = voiceId
        updatedCharacter.voiceDescription = voiceName
        updatedCharacter.updatedAt = Date()
        updateCharacter(updatedCharacter)
    }

    // MARK: - Relationships

    func addRelationship(to character: Character, relationship: CharacterRelationship) {
        var updatedCharacter = character
        updatedCharacter.relationships.append(relationship)
        updatedCharacter.updatedAt = Date()
        updateCharacter(updatedCharacter)
    }

    func removeRelationship(from character: Character, relationshipId: UUID) {
        var updatedCharacter = character
        updatedCharacter.relationships.removeAll { $0.id == relationshipId }
        updatedCharacter.updatedAt = Date()
        updateCharacter(updatedCharacter)
    }

    // MARK: - Helper Methods

    func getCharacter(by id: UUID) -> Character? {
        return characters.first { $0.id == id }
    }

    func getCharactersForStory(_ characterIds: [UUID]) -> [Character] {
        return characters.filter { characterIds.contains($0.id) }
    }
}
