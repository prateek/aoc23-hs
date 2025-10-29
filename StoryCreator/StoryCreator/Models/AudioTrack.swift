//
//  AudioTrack.swift
//  StoryCreator
//
//  Data model for audio tracks
//

import Foundation

struct AudioTrack: Identifiable, Codable, Hashable {
    var id: UUID
    var title: String
    var sceneId: UUID?
    var text: String
    var voiceId: String
    var voiceName: String
    var audioFileURL: URL?
    var localAudioPath: String?
    var duration: TimeInterval?
    var fileSize: Int64?
    var generationStatus: GenerationStatus
    var errorMessage: String?
    var createdAt: Date
    var updatedAt: Date

    init(
        id: UUID = UUID(),
        title: String = "",
        sceneId: UUID? = nil,
        text: String = "",
        voiceId: String = "",
        voiceName: String = "",
        audioFileURL: URL? = nil,
        localAudioPath: String? = nil,
        duration: TimeInterval? = nil,
        fileSize: Int64? = nil,
        generationStatus: GenerationStatus = .pending,
        errorMessage: String? = nil,
        createdAt: Date = Date(),
        updatedAt: Date = Date()
    ) {
        self.id = id
        self.title = title
        self.sceneId = sceneId
        self.text = text
        self.voiceId = voiceId
        self.voiceName = voiceName
        self.audioFileURL = audioFileURL
        self.localAudioPath = localAudioPath
        self.duration = duration
        self.fileSize = fileSize
        self.generationStatus = generationStatus
        self.errorMessage = errorMessage
        self.createdAt = createdAt
        self.updatedAt = updatedAt
    }
}

enum GenerationStatus: String, Codable, Hashable {
    case pending = "Pending"
    case generating = "Generating"
    case completed = "Completed"
    case failed = "Failed"
}
