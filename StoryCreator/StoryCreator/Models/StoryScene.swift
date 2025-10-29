//
//  StoryScene.swift
//  StoryCreator
//
//  Data model for storyboard scenes
//

import Foundation

struct StoryScene: Identifiable, Codable, Hashable {
    var id: UUID
    var orderIndex: Int
    var title: String
    var description: String
    var location: String
    var characterIds: [UUID]
    var plotPoints: [String]
    var emotion: String
    var notes: String
    var imagePrompt: String
    var generatedText: String?
    var audioTrackId: UUID?
    var duration: TimeInterval?
    var createdAt: Date
    var updatedAt: Date

    init(
        id: UUID = UUID(),
        orderIndex: Int = 0,
        title: String = "",
        description: String = "",
        location: String = "",
        characterIds: [UUID] = [],
        plotPoints: [String] = [],
        emotion: String = "",
        notes: String = "",
        imagePrompt: String = "",
        generatedText: String? = nil,
        audioTrackId: UUID? = nil,
        duration: TimeInterval? = nil,
        createdAt: Date = Date(),
        updatedAt: Date = Date()
    ) {
        self.id = id
        self.orderIndex = orderIndex
        self.title = title
        self.description = description
        self.location = location
        self.characterIds = characterIds
        self.plotPoints = plotPoints
        self.emotion = emotion
        self.notes = notes
        self.imagePrompt = imagePrompt
        self.generatedText = generatedText
        self.audioTrackId = audioTrackId
        self.duration = duration
        self.createdAt = createdAt
        self.updatedAt = updatedAt
    }
}

// Plot structure templates
enum PlotStructure: String, CaseIterable {
    case threeAct = "Three-Act Structure"
    case heroJourney = "Hero's Journey"
    case kishotenketsu = "Kishōtenketsu (4-Act)"
    case fivePart = "Five-Part Story Arc"
    case custom = "Custom"

    var defaultScenes: [String] {
        switch self {
        case .threeAct:
            return ["Setup", "Confrontation", "Resolution"]
        case .heroJourney:
            return ["Ordinary World", "Call to Adventure", "Crossing the Threshold",
                   "Tests and Trials", "Ordeal", "Return with Gift"]
        case .kishotenketsu:
            return ["Introduction (Ki)", "Development (Shō)", "Twist (Ten)", "Conclusion (Ketsu)"]
        case .fivePart:
            return ["Exposition", "Rising Action", "Climax", "Falling Action", "Resolution"]
        case .custom:
            return []
        }
    }
}
