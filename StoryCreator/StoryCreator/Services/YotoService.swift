//
//  YotoService.swift
//  StoryCreator
//
//  Service for Yoto API integration
//

import Foundation

class YotoService {
    static let shared = YotoService()
    private let config = APIConfiguration.shared
    private var authToken: String?

    private let baseURL = "https://api.yotoplay.com/v1"

    private init() {}

    // MARK: - Authentication

    func authenticate() async throws {
        guard !config.yotoEmail.isEmpty, !config.yotoPassword.isEmpty else {
            throw YotoError.missingCredentials
        }

        let url = URL(string: "\(baseURL)/auth/login")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.addValue("application/json", forHTTPHeaderField: "Content-Type")

        let body: [String: String] = [
            "email": config.yotoEmail,
            "password": config.yotoPassword
        ]

        request.httpBody = try JSONEncoder().encode(body)

        let (data, response) = try await URLSession.shared.data(for: request)

        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            throw YotoError.authenticationFailed
        }

        let authResponse = try JSONDecoder().decode(AuthResponse.self, from: data)
        self.authToken = authResponse.token
    }

    // MARK: - Playlist Creation

    func createPlaylist(name: String, description: String) async throws -> String {
        try await ensureAuthenticated()

        let url = URL(string: "\(baseURL)/playlists")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.addValue("Bearer \(authToken!)", forHTTPHeaderField: "Authorization")
        request.addValue("application/json", forHTTPHeaderField: "Content-Type")

        let body: [String: Any] = [
            "name": name,
            "description": description
        ]

        request.httpBody = try JSONSerialization.data(withJSONObject: body)

        let (data, response) = try await URLSession.shared.data(for: request)

        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            throw YotoError.playlistCreationFailed
        }

        let result = try JSONDecoder().decode(PlaylistResponse.self, from: data)
        return result.id
    }

    // MARK: - Track Upload

    func uploadTrack(
        playlistId: String,
        title: String,
        audioFilePath: String,
        order: Int
    ) async throws -> String {
        try await ensureAuthenticated()

        // Read audio file
        let audioURL = URL(fileURLWithPath: audioFilePath)
        let audioData = try Data(contentsOf: audioURL)

        // Validate file size (max 100 MB)
        let maxSize: Int64 = 100 * 1024 * 1024
        guard audioData.count <= maxSize else {
            throw YotoError.fileTooLarge
        }

        // Create upload request
        let boundary = UUID().uuidString
        let url = URL(string: "\(baseURL)/playlists/\(playlistId)/tracks")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.addValue("Bearer \(authToken!)", forHTTPHeaderField: "Authorization")
        request.addValue("multipart/form-data; boundary=\(boundary)", forHTTPHeaderField: "Content-Type")

        // Create multipart body
        var body = Data()

        // Add title
        body.append("--\(boundary)\r\n".data(using: .utf8)!)
        body.append("Content-Disposition: form-data; name=\"title\"\r\n\r\n".data(using: .utf8)!)
        body.append("\(title)\r\n".data(using: .utf8)!)

        // Add order
        body.append("--\(boundary)\r\n".data(using: .utf8)!)
        body.append("Content-Disposition: form-data; name=\"order\"\r\n\r\n".data(using: .utf8)!)
        body.append("\(order)\r\n".data(using: .utf8)!)

        // Add audio file
        body.append("--\(boundary)\r\n".data(using: .utf8)!)
        body.append("Content-Disposition: form-data; name=\"audio\"; filename=\"\(audioURL.lastPathComponent)\"\r\n".data(using: .utf8)!)
        body.append("Content-Type: audio/mpeg\r\n\r\n".data(using: .utf8)!)
        body.append(audioData)
        body.append("\r\n".data(using: .utf8)!)

        body.append("--\(boundary)--\r\n".data(using: .utf8)!)

        request.httpBody = body

        let (data, response) = try await URLSession.shared.data(for: request)

        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            if let errorString = String(data: data, encoding: .utf8) {
                print("Yoto Upload Error: \(errorString)")
            }
            throw YotoError.uploadFailed
        }

        let result = try JSONDecoder().decode(TrackResponse.self, from: data)
        return result.id
    }

    // MARK: - Complete Story Upload

    func uploadStoryToYoto(
        story: Story,
        audioTracks: [AudioTrack]
    ) async throws -> YotoPlaylist {
        // Validate track count (max 100)
        guard audioTracks.count <= 100 else {
            throw YotoError.tooManyTracks
        }

        // Validate total duration (max 5 hours)
        let totalDuration = audioTracks.compactMap { $0.duration }.reduce(0, +)
        guard totalDuration <= 18000 else { // 5 hours in seconds
            throw YotoError.playlistTooLong
        }

        // Create playlist
        let playlistId = try await createPlaylist(
            name: story.title,
            description: story.synopsis
        )

        var yotoTracks: [YotoTrack] = []

        // Upload each track
        for (index, audioTrack) in audioTracks.enumerated() {
            guard audioTrack.generationStatus == .completed,
                  let localPath = audioTrack.localAudioPath else {
                continue
            }

            do {
                let trackId = try await uploadTrack(
                    playlistId: playlistId,
                    title: audioTrack.title,
                    audioFilePath: localPath,
                    order: index + 1
                )

                let yotoTrack = YotoTrack(
                    audioTrackId: audioTrack.id,
                    title: audioTrack.title,
                    order: index + 1,
                    localPath: localPath,
                    uploadedURL: trackId,
                    duration: audioTrack.duration ?? 0
                )

                yotoTracks.append(yotoTrack)
            } catch {
                print("Failed to upload track \(audioTrack.title): \(error)")
                throw error
            }
        }

        return YotoPlaylist(
            id: playlistId,
            name: story.title,
            description: story.synopsis,
            storyId: story.id,
            tracks: yotoTracks,
            totalDuration: totalDuration,
            uploadStatus: .completed
        )
    }

    // MARK: - Fetch User Playlists

    func fetchUserPlaylists() async throws -> [YotoPlaylistInfo] {
        try await ensureAuthenticated()

        let url = URL(string: "\(baseURL)/playlists")!
        var request = URLRequest(url: url)
        request.addValue("Bearer \(authToken!)", forHTTPHeaderField: "Authorization")

        let (data, response) = try await URLSession.shared.data(for: request)

        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            throw YotoError.fetchFailed
        }

        let result = try JSONDecoder().decode(PlaylistsResponse.self, from: data)
        return result.playlists
    }

    // MARK: - Helper Methods

    private func ensureAuthenticated() async throws {
        if authToken == nil {
            try await authenticate()
        }
    }
}

// MARK: - Response Models

struct AuthResponse: Codable {
    let token: String
}

struct PlaylistResponse: Codable {
    let id: String
    let name: String
}

struct TrackResponse: Codable {
    let id: String
}

struct PlaylistsResponse: Codable {
    let playlists: [YotoPlaylistInfo]
}

struct YotoPlaylistInfo: Codable, Identifiable {
    let id: String
    let name: String
    let description: String?
    let trackCount: Int
    let createdAt: String

    enum CodingKeys: String, CodingKey {
        case id
        case name
        case description
        case trackCount = "track_count"
        case createdAt = "created_at"
    }
}

// MARK: - Errors

enum YotoError: LocalizedError {
    case missingCredentials
    case authenticationFailed
    case playlistCreationFailed
    case uploadFailed
    case fetchFailed
    case fileTooLarge
    case tooManyTracks
    case playlistTooLong

    var errorDescription: String? {
        switch self {
        case .missingCredentials:
            return "Yoto credentials are missing. Please configure them in settings."
        case .authenticationFailed:
            return "Failed to authenticate with Yoto. Please check your credentials."
        case .playlistCreationFailed:
            return "Failed to create playlist on Yoto."
        case .uploadFailed:
            return "Failed to upload track to Yoto."
        case .fetchFailed:
            return "Failed to fetch playlists from Yoto."
        case .fileTooLarge:
            return "Audio file is too large. Maximum size is 100 MB per track."
        case .tooManyTracks:
            return "Too many tracks. Maximum is 100 tracks per playlist."
        case .playlistTooLong:
            return "Playlist is too long. Maximum total duration is 5 hours."
        }
    }
}
