//
//  LibraryViewModel.swift
//  StoryCreator
//
//  ViewModel for story library management
//

import Foundation
import Combine

@MainActor
class LibraryViewModel: ObservableObject {
    @Published var stories: [Story] = []
    @Published var characters: [Character] = []
    @Published var filteredStories: [Story] = []
    @Published var searchText: String = "" {
        didSet {
            filterStories()
        }
    }
    @Published var selectedGenreFilter: StoryGenre?
    @Published var selectedAgeFilter: AgeRange?

    private let storageKey = "SavedStories"
    private let charactersKey = "SavedCharacters"

    init() {
        loadFromStorage()
        filterStories()
    }

    // MARK: - Story Management

    func addStory(_ story: Story) {
        stories.append(story)
        saveToStorage()
        filterStories()
    }

    func updateStory(_ story: Story) {
        if let index = stories.firstIndex(where: { $0.id == story.id }) {
            stories[index] = story
            saveToStorage()
            filterStories()
        }
    }

    func deleteStory(_ story: Story) {
        // Delete associated audio files
        for audioTrack in story.audioTracks {
            if let localPath = audioTrack.localAudioPath {
                try? FileManager.default.removeItem(atPath: localPath)
            }
        }

        stories.removeAll { $0.id == story.id }
        saveToStorage()
        filterStories()
    }

    func duplicateStory(_ story: Story) {
        var newStory = story
        newStory.id = UUID()
        newStory.title = "\(story.title) (Copy)"
        newStory.yotoPlaylistId = nil
        newStory.audioTracks = []
        newStory.createdAt = Date()
        newStory.updatedAt = Date()
        addStory(newStory)
    }

    // MARK: - Character Management

    func addCharacter(_ character: Character) {
        characters.append(character)
        saveToStorage()
    }

    func updateCharacter(_ character: Character) {
        if let index = characters.firstIndex(where: { $0.id == character.id }) {
            characters[index] = character
            saveToStorage()
        }
    }

    func deleteCharacter(_ character: Character) {
        characters.removeAll { $0.id == character.id }
        saveToStorage()
    }

    // MARK: - Filtering

    func filterStories() {
        var filtered = stories

        // Text search
        if !searchText.isEmpty {
            filtered = filtered.filter { story in
                story.title.localizedCaseInsensitiveContains(searchText) ||
                story.synopsis.localizedCaseInsensitiveContains(searchText) ||
                story.theme.localizedCaseInsensitiveContains(searchText)
            }
        }

        // Genre filter
        if let genre = selectedGenreFilter {
            filtered = filtered.filter { $0.genre == genre }
        }

        // Age filter
        if let age = selectedAgeFilter {
            filtered = filtered.filter { $0.targetAge == age }
        }

        filteredStories = filtered.sorted { $0.updatedAt > $1.updatedAt }
    }

    func clearFilters() {
        searchText = ""
        selectedGenreFilter = nil
        selectedAgeFilter = nil
        filterStories()
    }

    // MARK: - Storage

    private func saveToStorage() {
        // Save stories
        if let encoded = try? JSONEncoder().encode(stories) {
            UserDefaults.standard.set(encoded, forKey: storageKey)
        }

        // Save characters
        if let encoded = try? JSONEncoder().encode(characters) {
            UserDefaults.standard.set(encoded, forKey: charactersKey)
        }
    }

    private func loadFromStorage() {
        // Load stories
        if let data = UserDefaults.standard.data(forKey: storageKey),
           let decoded = try? JSONDecoder().decode([Story].self, from: data) {
            stories = decoded
        }

        // Load characters
        if let data = UserDefaults.standard.data(forKey: charactersKey),
           let decoded = try? JSONDecoder().decode([Character].self, from: data) {
            characters = decoded
        }
    }

    // MARK: - Statistics

    func getTotalStories() -> Int {
        return stories.count
    }

    func getTotalCharacters() -> Int {
        return characters.count
    }

    func getStoriesWithAudio() -> Int {
        return stories.filter { !$0.audioTracks.isEmpty }.count
    }

    func getStoriesOnYoto() -> Int {
        return stories.filter { $0.yotoPlaylistId != nil }.count
    }
}
