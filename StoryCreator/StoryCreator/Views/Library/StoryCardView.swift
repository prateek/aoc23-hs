//
//  StoryCardView.swift
//  StoryCreator
//
//  Card view for displaying story summaries
//

import SwiftUI

struct StoryCardView: View {
    let story: Story

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            // Header
            HStack {
                VStack(alignment: .leading, spacing: 4) {
                    Text(story.title)
                        .font(.headline)
                        .foregroundColor(.primary)

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
                    }
                }

                Spacer()

                if story.yotoPlaylistId != nil {
                    Image(systemName: "checkmark.circle.fill")
                        .foregroundColor(.green)
                        .font(.title2)
                }
            }

            // Synopsis
            if !story.synopsis.isEmpty {
                Text(story.synopsis)
                    .font(.subheadline)
                    .foregroundColor(.secondary)
                    .lineLimit(3)
            }

            // Stats
            HStack(spacing: 16) {
                Label("\(story.scenes.count)", systemImage: "list.bullet")
                    .font(.caption)
                    .foregroundColor(.secondary)

                Label("\(story.characterIds.count)", systemImage: "person.2")
                    .font(.caption)
                    .foregroundColor(.secondary)

                if !story.audioTracks.isEmpty {
                    Label("\(story.audioTracks.count)", systemImage: "speaker.wave.2")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }

                Spacer()

                Text(story.updatedAt.formatted(date: .abbreviated, time: .omitted))
                    .font(.caption2)
                    .foregroundColor(.secondary)
            }
        }
        .padding()
        .background(Color(.systemGray6))
        .cornerRadius(12)
    }
}

#Preview {
    StoryCardView(story: Story(
        title: "The Magical Forest Adventure",
        genre: .fantasy,
        targetAge: .earlyElementary,
        synopsis: "Join Emma as she discovers a magical forest filled with talking animals and enchanted treasures.",
        scenes: [StoryScene(), StoryScene(), StoryScene()],
        audioTracks: [AudioTrack()]
    ))
    .padding()
}
