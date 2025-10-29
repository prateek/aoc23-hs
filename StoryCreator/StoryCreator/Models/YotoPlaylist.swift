//
//  YotoPlaylist.swift
//  StoryCreator
//
//  Data model for Yoto playlists
//

import Foundation

struct YotoPlaylist: Identifiable, Codable {
    var id: String
    var name: String
    var description: String
    var storyId: UUID
    var tracks: [YotoTrack]
    var totalDuration: TimeInterval
    var uploadStatus: UploadStatus
    var createdAt: Date
    var updatedAt: Date

    init(
        id: String = UUID().uuidString,
        name: String = "",
        description: String = "",
        storyId: UUID,
        tracks: [YotoTrack] = [],
        totalDuration: TimeInterval = 0,
        uploadStatus: UploadStatus = .notStarted,
        createdAt: Date = Date(),
        updatedAt: Date = Date()
    ) {
        self.id = id
        self.name = name
        self.description = description
        self.storyId = storyId
        self.tracks = tracks
        self.totalDuration = totalDuration
        self.uploadStatus = uploadStatus
        self.createdAt = createdAt
        self.updatedAt = updatedAt
    }
}

struct YotoTrack: Identifiable, Codable, Hashable {
    var id: UUID
    var audioTrackId: UUID
    var title: String
    var order: Int
    var localPath: String
    var uploadedURL: String?
    var duration: TimeInterval

    init(
        id: UUID = UUID(),
        audioTrackId: UUID,
        title: String,
        order: Int,
        localPath: String,
        uploadedURL: String? = nil,
        duration: TimeInterval
    ) {
        self.id = id
        self.audioTrackId = audioTrackId
        self.title = title
        self.order = order
        self.localPath = localPath
        self.uploadedURL = uploadedURL
        self.duration = duration
    }
}

enum UploadStatus: String, Codable {
    case notStarted = "Not Started"
    case uploading = "Uploading"
    case completed = "Completed"
    case failed = "Failed"
}

// Yoto API response models
struct YotoPlaylistResponse: Codable {
    let id: String
    let name: String
    let tracks: [YotoTrackResponse]
}

struct YotoTrackResponse: Codable {
    let id: String
    let title: String
    let url: String
}
