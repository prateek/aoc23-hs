//
//  YotoUploadView.swift
//  StoryCreator
//
//  Yoto upload interface
//

import SwiftUI

struct YotoUploadView: View {
    @Environment(\.dismiss) var dismiss
    @State var story: Story
    @State private var isUploading = false
    @State private var uploadProgress: Double = 0.0
    @State private var uploadComplete = false
    @State private var errorMessage: String?

    private let yotoService = YotoService.shared

    var body: some View {
        NavigationView {
            VStack(spacing: 30) {
                // Header
                VStack(spacing: 12) {
                    Image(systemName: "square.and.arrow.up.circle.fill")
                        .font(.system(size: 60))
                        .foregroundColor(.blue)

                    Text("Upload to Yoto")
                        .font(.title2)
                        .bold()

                    Text("Create a playlist on your Yoto player")
                        .font(.subheadline)
                        .foregroundColor(.secondary)
                        .multilineTextAlignment(.center)
                }
                .padding()

                // Story info
                VStack(alignment: .leading, spacing: 12) {
                    InfoRow(label: "Story", value: story.title)
                    InfoRow(label: "Tracks", value: "\(story.audioTracks.count)")
                    InfoRow(label: "Total Duration", value: formatTotalDuration())
                }
                .padding()
                .background(Color(.systemGray6))
                .cornerRadius(12)
                .padding(.horizontal)

                // Upload status
                if isUploading {
                    VStack(spacing: 12) {
                        ProgressView(value: uploadProgress)
                            .progressViewStyle(.linear)

                        Text("Uploading... \(Int(uploadProgress * 100))%")
                            .font(.subheadline)
                            .foregroundColor(.secondary)
                    }
                    .padding()
                } else if uploadComplete {
                    VStack(spacing: 12) {
                        Image(systemName: "checkmark.circle.fill")
                            .font(.system(size: 60))
                            .foregroundColor(.green)

                        Text("Upload Complete!")
                            .font(.headline)

                        Text("Your story is now on Yoto")
                            .font(.subheadline)
                            .foregroundColor(.secondary)
                    }
                    .padding()
                } else if let error = errorMessage {
                    VStack(spacing: 12) {
                        Image(systemName: "exclamationmark.triangle.fill")
                            .font(.system(size: 60))
                            .foregroundColor(.red)

                        Text("Upload Failed")
                            .font(.headline)

                        Text(error)
                            .font(.subheadline)
                            .foregroundColor(.secondary)
                            .multilineTextAlignment(.center)
                    }
                    .padding()
                }

                Spacer()

                // Action buttons
                VStack(spacing: 12) {
                    if !isUploading && !uploadComplete {
                        Button(action: { Task { await uploadToYoto() } }) {
                            Label("Start Upload", systemImage: "square.and.arrow.up")
                                .font(.headline)
                                .foregroundColor(.white)
                                .frame(maxWidth: .infinity)
                                .padding()
                                .background(Color.blue)
                                .cornerRadius(10)
                        }
                        .disabled(story.audioTracks.isEmpty)
                    }

                    if uploadComplete || errorMessage != nil {
                        Button(action: { dismiss() }) {
                            Text("Done")
                                .font(.headline)
                                .frame(maxWidth: .infinity)
                                .padding()
                        }
                        .buttonStyle(.bordered)
                    }
                }
                .padding()
            }
            .navigationTitle("Upload to Yoto")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarLeading) {
                    if !isUploading {
                        Button("Cancel") {
                            dismiss()
                        }
                    }
                }
            }
        }
    }

    private func uploadToYoto() async {
        isUploading = true
        uploadProgress = 0.0
        errorMessage = nil

        do {
            // Simulate progress updates
            for i in 1...10 {
                try await Task.sleep(nanoseconds: 300_000_000)
                uploadProgress = Double(i) / 10.0
            }

            let playlist = try await yotoService.uploadStoryToYoto(
                story: story,
                audioTracks: story.audioTracks
            )

            story.yotoPlaylistId = playlist.id
            uploadComplete = true
        } catch {
            errorMessage = error.localizedDescription
        }

        isUploading = false
    }

    private func formatTotalDuration() -> String {
        let total = story.audioTracks.compactMap { $0.duration }.reduce(0, +)
        let minutes = Int(total) / 60
        let seconds = Int(total) % 60
        return String(format: "%d:%02d", minutes, seconds)
    }
}

struct InfoRow: View {
    let label: String
    let value: String

    var body: some View {
        HStack {
            Text(label)
                .font(.subheadline)
                .foregroundColor(.secondary)
            Spacer()
            Text(value)
                .font(.subheadline)
                .bold()
        }
    }
}

#Preview {
    YotoUploadView(story: Story(
        title: "Test Story",
        audioTracks: [AudioTrack(), AudioTrack()]
    ))
}
