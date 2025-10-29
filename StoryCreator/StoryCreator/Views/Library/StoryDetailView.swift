//
//  StoryDetailView.swift
//  StoryCreator
//
//  Detailed story view with actions
//

import SwiftUI
import AVFoundation

struct StoryDetailView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @Environment(\.dismiss) var dismiss
    @State var story: Story
    @State private var showingYotoUpload = false
    @State private var showingShareSheet = false
    @State private var selectedTab = 0

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 20) {
                // Header
                headerView

                // Tabs
                Picker("View", selection: $selectedTab) {
                    Text("Story").tag(0)
                    Text("Scenes").tag(1)
                    Text("Audio").tag(2)
                }
                .pickerStyle(.segmented)
                .padding(.horizontal)

                // Content
                if selectedTab == 0 {
                    storyTab
                } else if selectedTab == 1 {
                    scenesTab
                } else {
                    audioTab
                }

                // Actions
                actionButtons
            }
            .padding()
        }
        .navigationTitle(story.title)
        .navigationBarTitleDisplayMode(.inline)
        .sheet(isPresented: $showingYotoUpload) {
            YotoUploadView(story: story)
        }
    }

    private var headerView: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack {
                Text(story.genre.rawValue)
                    .font(.caption)
                    .foregroundColor(.white)
                    .padding(.horizontal, 8)
                    .padding(.vertical, 4)
                    .background(Color.blue)
                    .cornerRadius(4)

                Text(story.targetAge.rawValue)
                    .font(.caption)
                    .foregroundColor(.secondary)

                Spacer()

                if story.yotoPlaylistId != nil {
                    Label("On Yoto", systemImage: "checkmark.circle.fill")
                        .font(.caption)
                        .foregroundColor(.green)
                }
            }

            if !story.synopsis.isEmpty {
                Text(story.synopsis)
                    .font(.subheadline)
                    .foregroundColor(.secondary)
            }

            HStack {
                if !story.theme.isEmpty {
                    Label(story.theme, systemImage: "sparkles")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }

                if !story.setting.isEmpty {
                    Label(story.setting, systemImage: "location")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
            }
        }
        .padding()
        .background(Color(.systemGray6))
        .cornerRadius(12)
    }

    private var storyTab: some View {
        VStack(alignment: .leading, spacing: 12) {
            if let generatedText = story.generatedText {
                Text(generatedText)
                    .padding()
            } else {
                Text("Story text not generated yet")
                    .foregroundColor(.secondary)
                    .padding()
            }
        }
    }

    private var scenesTab: some View {
        VStack(spacing: 12) {
            ForEach(Array(story.scenes.enumerated()), id: \.element.id) { index, scene in
                VStack(alignment: .leading, spacing: 8) {
                    HStack {
                        Text("Scene \(index + 1)")
                            .font(.caption)
                            .foregroundColor(.white)
                            .padding(.horizontal, 8)
                            .padding(.vertical, 4)
                            .background(Color.blue)
                            .cornerRadius(4)

                        Text(scene.title)
                            .font(.headline)

                        Spacer()

                        if scene.generatedText != nil {
                            Image(systemName: "checkmark.circle.fill")
                                .foregroundColor(.green)
                        }
                    }

                    if let text = scene.generatedText {
                        Text(text)
                            .font(.subheadline)
                            .foregroundColor(.secondary)
                    }
                }
                .padding()
                .background(Color(.systemGray6))
                .cornerRadius(10)
            }
        }
    }

    private var audioTab: some View {
        VStack(spacing: 12) {
            if story.audioTracks.isEmpty {
                Text("No audio tracks generated yet")
                    .foregroundColor(.secondary)
                    .padding()
            } else {
                ForEach(story.audioTracks) { track in
                    AudioTrackRow(track: track)
                }
            }
        }
    }

    private var actionButtons: some View {
        VStack(spacing: 12) {
            if !story.audioTracks.isEmpty && story.yotoPlaylistId == nil {
                Button(action: { showingYotoUpload = true }) {
                    Label("Upload to Yoto", systemImage: "square.and.arrow.up")
                        .font(.headline)
                        .foregroundColor(.white)
                        .frame(maxWidth: .infinity)
                        .padding()
                        .background(Color.green)
                        .cornerRadius(10)
                }
            }

            Button(action: { showingShareSheet = true }) {
                Label("Share Story", systemImage: "square.and.arrow.up")
                    .font(.headline)
                    .frame(maxWidth: .infinity)
                    .padding()
            }
            .buttonStyle(.bordered)
        }
        .padding(.horizontal)
    }
}

struct AudioTrackRow: View {
    let track: AudioTrack
    @State private var isPlaying = false
    @State private var audioPlayer: AVAudioPlayer?

    var body: some View {
        HStack {
            Button(action: togglePlayback) {
                Image(systemName: isPlaying ? "pause.circle.fill" : "play.circle.fill")
                    .font(.title2)
                    .foregroundColor(.blue)
            }

            VStack(alignment: .leading) {
                Text(track.title)
                    .font(.subheadline)

                if let duration = track.duration {
                    Text(formatDuration(duration))
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
            }

            Spacer()

            if track.generationStatus == .completed {
                Image(systemName: "checkmark.circle.fill")
                    .foregroundColor(.green)
            } else if track.generationStatus == .generating {
                ProgressView()
            } else if track.generationStatus == .failed {
                Image(systemName: "exclamationmark.circle.fill")
                    .foregroundColor(.red)
            }
        }
        .padding()
        .background(Color(.systemGray6))
        .cornerRadius(10)
    }

    private func togglePlayback() {
        guard let localPath = track.localAudioPath else { return }

        if isPlaying {
            audioPlayer?.stop()
            isPlaying = false
        } else {
            do {
                let url = URL(fileURLWithPath: localPath)
                audioPlayer = try AVAudioPlayer(contentsOf: url)
                audioPlayer?.play()
                isPlaying = true
            } catch {
                print("Failed to play audio: \(error)")
            }
        }
    }

    private func formatDuration(_ duration: TimeInterval) -> String {
        let minutes = Int(duration) / 60
        let seconds = Int(duration) % 60
        return String(format: "%d:%02d", minutes, seconds)
    }
}

#Preview {
    NavigationView {
        StoryDetailView(story: Story(
            title: "The Magical Forest",
            genre: .fantasy,
            targetAge: .earlyElementary,
            synopsis: "An adventure through a magical forest",
            scenes: [StoryScene(), StoryScene()],
            audioTracks: [AudioTrack()]
        ))
        .environmentObject(LibraryViewModel())
    }
}
