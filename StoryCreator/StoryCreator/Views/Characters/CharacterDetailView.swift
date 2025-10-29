//
//  CharacterDetailView.swift
//  StoryCreator
//
//  Detailed character view
//

import SwiftUI

struct CharacterDetailView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @Environment(\.dismiss) var dismiss
    @State var character: Character
    @State private var showingEditSheet = false

    var body: some View {
        NavigationView {
            ScrollView {
                VStack(alignment: .leading, spacing: 20) {
                    // Header
                    VStack(spacing: 12) {
                        ZStack {
                            Circle()
                                .fill(Color.blue.opacity(0.2))
                                .frame(width: 100, height: 100)

                            Text(character.name.prefix(1).uppercased())
                                .font(.system(size: 48))
                                .bold()
                                .foregroundColor(.blue)
                        }

                        Text(character.name)
                            .font(.title)
                            .bold()

                        HStack {
                            Text(character.role.rawValue)
                                .font(.subheadline)
                                .foregroundColor(.white)
                                .padding(.horizontal, 12)
                                .padding(.vertical, 6)
                                .background(Color.blue)
                                .cornerRadius(8)

                            if !character.age.isEmpty {
                                Text(character.age)
                                    .font(.subheadline)
                                    .foregroundColor(.secondary)
                            }
                        }
                    }
                    .frame(maxWidth: .infinity)
                    .padding(.vertical)

                    // Sections
                    if !character.appearance.isEmpty {
                        SectionView(title: "Appearance", icon: "eye", content: character.appearance)
                    }

                    if !character.personality.isEmpty {
                        SectionView(title: "Personality", icon: "sparkles", content: character.personality)
                    }

                    if !character.backstory.isEmpty {
                        SectionView(title: "Backstory", icon: "book", content: character.backstory)
                    }

                    if character.elevenLabsVoiceId != nil {
                        VStack(alignment: .leading, spacing: 8) {
                            Label("Voice", systemImage: "speaker.wave.2")
                                .font(.headline)

                            Text(character.voiceDescription.isEmpty ? "Voice assigned" : character.voiceDescription)
                                .foregroundColor(.secondary)
                        }
                        .padding()
                        .frame(maxWidth: .infinity, alignment: .leading)
                        .background(Color(.systemGray6))
                        .cornerRadius(10)
                    }

                    if !character.relationships.isEmpty {
                        VStack(alignment: .leading, spacing: 12) {
                            Label("Relationships", systemImage: "person.2")
                                .font(.headline)

                            ForEach(character.relationships) { relationship in
                                HStack {
                                    VStack(alignment: .leading) {
                                        Text(relationship.characterName)
                                            .font(.subheadline)
                                        Text(relationship.relationshipType.rawValue)
                                            .font(.caption)
                                            .foregroundColor(.secondary)
                                    }
                                    Spacer()
                                }
                                .padding()
                                .background(Color(.systemGray6))
                                .cornerRadius(8)
                            }
                        }
                    }
                }
                .padding()
            }
            .navigationTitle("Character Details")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarLeading) {
                    Button("Done") {
                        dismiss()
                    }
                }

                ToolbarItem(placement: .navigationBarTrailing) {
                    Button("Edit") {
                        showingEditSheet = true
                    }
                }
            }
            .sheet(isPresented: $showingEditSheet) {
                CharacterCreateView(character: character)
            }
        }
    }
}

struct SectionView: View {
    let title: String
    let icon: String
    let content: String

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Label(title, systemImage: icon)
                .font(.headline)

            Text(content)
                .foregroundColor(.secondary)
        }
        .padding()
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(Color(.systemGray6))
        .cornerRadius(10)
    }
}

#Preview {
    CharacterDetailView(character: Character(
        name: "Emma the Explorer",
        age: "8 years old",
        role: .protagonist,
        appearance: "Bright blue eyes, curly red hair, always wears her lucky green backpack",
        personality: "Curious, brave, and kind-hearted",
        backstory: "Emma grew up in a small village but always dreamed of adventures beyond the hills."
    ))
    .environmentObject(LibraryViewModel())
}
