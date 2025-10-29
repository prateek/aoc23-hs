//
//  Story.swift
//  StoryCreator
//
//  Data model for stories
//

import Foundation

struct Story: Identifiable, Codable {
    var id: UUID
    var title: String
    var genre: StoryGenre
    var targetAge: AgeRange
    var synopsis: String
    var theme: String
    var setting: String
    var mood: String
    var characterIds: [UUID]
    var scenes: [StoryScene]
    var generatedText: String?
    var audioTracks: [AudioTrack]
    var yotoPlaylistId: String?
    var coverImageData: Data?
    var createdAt: Date
    var updatedAt: Date

    init(
        id: UUID = UUID(),
        title: String = "",
        genre: StoryGenre = .adventure,
        targetAge: AgeRange = .preschool,
        synopsis: String = "",
        theme: String = "",
        setting: String = "",
        mood: String = "",
        characterIds: [UUID] = [],
        scenes: [StoryScene] = [],
        generatedText: String? = nil,
        audioTracks: [AudioTrack] = [],
        yotoPlaylistId: String? = nil,
        coverImageData: Data? = nil,
        createdAt: Date = Date(),
        updatedAt: Date = Date()
    ) {
        self.id = id
        self.title = title
        self.genre = genre
        self.targetAge = targetAge
        self.synopsis = synopsis
        self.theme = theme
        self.setting = setting
        self.mood = mood
        self.characterIds = characterIds
        self.scenes = scenes
        self.generatedText = generatedText
        self.audioTracks = audioTracks
        self.yotoPlaylistId = yotoPlaylistId
        self.coverImageData = coverImageData
        self.createdAt = createdAt
        self.updatedAt = updatedAt
    }
}

enum StoryGenre: String, Codable, CaseIterable {
    case adventure = "Adventure"
    case fantasy = "Fantasy"
    case scienceFiction = "Science Fiction"
    case mystery = "Mystery"
    case educational = "Educational"
    case fairytale = "Fairy Tale"
    case animal = "Animal Story"
    case bedtime = "Bedtime Story"
    case friendship = "Friendship"
    case family = "Family"
    case humor = "Humor"
    case other = "Other"
}

enum AgeRange: String, Codable, CaseIterable {
    case preschool = "Preschool (2-4 years)"
    case earlyElementary = "Early Elementary (5-7 years)"
    case elementary = "Elementary (8-10 years)"
    case preteen = "Preteen (11-12 years)"

    var description: String {
        return self.rawValue
    }
}
