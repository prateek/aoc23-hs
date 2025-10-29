//
//  Character.swift
//  StoryCreator
//
//  Data model for story characters
//

import Foundation
import SwiftUI

struct Character: Identifiable, Codable, Hashable {
    var id: UUID
    var name: String
    var age: String
    var role: CharacterRole
    var appearance: String
    var personality: String
    var backstory: String
    var voiceDescription: String
    var elevenLabsVoiceId: String?
    var relationships: [CharacterRelationship]
    var imageData: Data?
    var createdAt: Date
    var updatedAt: Date

    init(
        id: UUID = UUID(),
        name: String = "",
        age: String = "",
        role: CharacterRole = .supporting,
        appearance: String = "",
        personality: String = "",
        backstory: String = "",
        voiceDescription: String = "",
        elevenLabsVoiceId: String? = nil,
        relationships: [CharacterRelationship] = [],
        imageData: Data? = nil,
        createdAt: Date = Date(),
        updatedAt: Date = Date()
    ) {
        self.id = id
        self.name = name
        self.age = age
        self.role = role
        self.appearance = appearance
        self.personality = personality
        self.backstory = backstory
        self.voiceDescription = voiceDescription
        self.elevenLabsVoiceId = elevenLabsVoiceId
        self.relationships = relationships
        self.imageData = imageData
        self.createdAt = createdAt
        self.updatedAt = updatedAt
    }
}

enum CharacterRole: String, Codable, CaseIterable {
    case protagonist = "Protagonist"
    case antagonist = "Antagonist"
    case supporting = "Supporting"
    case sidekick = "Sidekick"
    case mentor = "Mentor"
    case other = "Other"
}

struct CharacterRelationship: Identifiable, Codable, Hashable {
    var id: UUID
    var characterId: UUID
    var characterName: String
    var relationshipType: RelationType
    var description: String

    init(
        id: UUID = UUID(),
        characterId: UUID,
        characterName: String,
        relationshipType: RelationType,
        description: String = ""
    ) {
        self.id = id
        self.characterId = characterId
        self.characterName = characterName
        self.relationshipType = relationshipType
        self.description = description
    }
}

enum RelationType: String, Codable, CaseIterable {
    case parent = "Parent"
    case child = "Child"
    case sibling = "Sibling"
    case friend = "Friend"
    case enemy = "Enemy"
    case rival = "Rival"
    case mentor = "Mentor"
    case student = "Student"
    case romantic = "Romantic Interest"
    case other = "Other"
}
