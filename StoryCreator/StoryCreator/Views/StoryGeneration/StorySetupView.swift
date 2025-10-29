//
//  StorySetupView.swift
//  StoryCreator
//
//  Initial story setup view
//

import SwiftUI

struct StorySetupView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @Environment(\.dismiss) var dismiss
    @State private var story = Story()
    @State private var selectedCharacterIds: Set<UUID> = []
    @State private var navigateToStoryboard = false

    var body: some View {
        NavigationView {
            Form {
                Section("Story Details") {
                    TextField("Story Title", text: $story.title)

                    Picker("Genre", selection: $story.genre) {
                        ForEach(StoryGenre.allCases, id: \.self) { genre in
                            Text(genre.rawValue).tag(genre)
                        }
                    }

                    Picker("Target Age", selection: $story.targetAge) {
                        ForEach(AgeRange.allCases, id: \.self) { age in
                            Text(age.description).tag(age)
                        }
                    }
                }

                Section("Story Concept") {
                    TextEditor(text: $story.synopsis)
                        .frame(minHeight: 80)
                        .overlay(
                            Text(story.synopsis.isEmpty ? "Brief synopsis of your story..." : "")
                                .foregroundColor(.gray.opacity(0.5))
                                .padding(.top, 8)
                                .padding(.leading, 4)
                                .allowsHitTesting(false),
                            alignment: .topLeading
                        )

                    TextField("Theme (e.g., friendship, courage)", text: $story.theme)
                    TextField("Setting", text: $story.setting)
                    TextField("Mood (e.g., adventurous, calm)", text: $story.mood)
                }

                Section("Characters") {
                    if libraryViewModel.characters.isEmpty {
                        Text("No characters available. Create some first!")
                            .foregroundColor(.secondary)
                            .font(.caption)
                    } else {
                        ForEach(libraryViewModel.characters) { character in
                            Toggle(isOn: Binding(
                                get: { selectedCharacterIds.contains(character.id) },
                                set: { isSelected in
                                    if isSelected {
                                        selectedCharacterIds.insert(character.id)
                                    } else {
                                        selectedCharacterIds.remove(character.id)
                                    }
                                }
                            )) {
                                VStack(alignment: .leading) {
                                    Text(character.name)
                                    Text(character.role.rawValue)
                                        .font(.caption)
                                        .foregroundColor(.secondary)
                                }
                            }
                        }
                    }
                }
            }
            .navigationTitle("New Story")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarLeading) {
                    Button("Cancel") {
                        dismiss()
                    }
                }

                ToolbarItem(placement: .navigationBarTrailing) {
                    Button("Next") {
                        saveAndContinue()
                    }
                    .disabled(story.title.isEmpty || selectedCharacterIds.isEmpty)
                }
            }
            .navigationDestination(isPresented: $navigateToStoryboard) {
                StoryboardView(story: story, selectedCharacters: getSelectedCharacters())
            }
        }
    }

    private func getSelectedCharacters() -> [Character] {
        return libraryViewModel.characters.filter { selectedCharacterIds.contains($0.id) }
    }

    private func saveAndContinue() {
        story.characterIds = Array(selectedCharacterIds)
        navigateToStoryboard = true
    }
}

#Preview {
    StorySetupView()
        .environmentObject(LibraryViewModel())
}
