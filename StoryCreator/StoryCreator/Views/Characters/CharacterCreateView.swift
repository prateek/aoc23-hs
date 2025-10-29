//
//  CharacterCreateView.swift
//  StoryCreator
//
//  Character creation/edit view
//

import SwiftUI

struct CharacterCreateView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @Environment(\.dismiss) var dismiss
    @StateObject private var viewModel = CharacterViewModel()

    @State var character: Character
    @State private var showingVoicePicker = false

    var body: some View {
        NavigationView {
            Form {
                Section("Basic Information") {
                    TextField("Character Name", text: $character.name)
                    TextField("Age", text: $character.age)

                    Picker("Role", selection: $character.role) {
                        ForEach(CharacterRole.allCases, id: \.self) { role in
                            Text(role.rawValue).tag(role)
                        }
                    }
                }

                Section("Physical Description") {
                    TextEditor(text: $character.appearance)
                        .frame(minHeight: 100)

                    if viewModel.isLoading {
                        ProgressView()
                    } else {
                        Button("Enhance with AI") {
                            Task {
                                await viewModel.enhanceCharacterDescription(for: character)
                                if let enhanced = viewModel.characters.first(where: { $0.id == character.id }) {
                                    character = enhanced
                                }
                            }
                        }
                    }
                }

                Section("Personality") {
                    TextEditor(text: $character.personality)
                        .frame(minHeight: 80)
                }

                Section("Backstory") {
                    TextEditor(text: $character.backstory)
                        .frame(minHeight: 100)
                }

                Section("Voice") {
                    if let voiceId = character.elevenLabsVoiceId {
                        HStack {
                            Text("Voice Selected")
                            Spacer()
                            Image(systemName: "checkmark.circle.fill")
                                .foregroundColor(.green)
                        }
                    } else {
                        Text("No voice selected")
                            .foregroundColor(.secondary)
                    }

                    Button("Select Voice") {
                        showingVoicePicker = true
                    }
                }

                if viewModel.errorMessage != nil {
                    Section {
                        Text(viewModel.errorMessage!)
                            .foregroundColor(.red)
                            .font(.caption)
                    }
                }
            }
            .navigationTitle(character.name.isEmpty ? "New Character" : character.name)
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarLeading) {
                    Button("Cancel") {
                        dismiss()
                    }
                }

                ToolbarItem(placement: .navigationBarTrailing) {
                    Button("Save") {
                        saveCharacter()
                    }
                    .disabled(character.name.isEmpty)
                }
            }
            .sheet(isPresented: $showingVoicePicker) {
                VoicePickerView(character: $character)
            }
        }
    }

    private func saveCharacter() {
        character.updatedAt = Date()
        libraryViewModel.addCharacter(character)
        dismiss()
    }
}

struct VoicePickerView: View {
    @Binding var character: Character
    @Environment(\.dismiss) var dismiss
    @StateObject private var viewModel = CharacterViewModel()

    var body: some View {
        NavigationView {
            Group {
                if viewModel.isLoading {
                    ProgressView("Loading voices...")
                } else if viewModel.availableVoices.isEmpty {
                    VStack {
                        Text("No voices available")
                        Text("Please check your ElevenLabs API key")
                            .font(.caption)
                            .foregroundColor(.secondary)
                    }
                } else {
                    List(viewModel.availableVoices) { voice in
                        Button(action: {
                            character.elevenLabsVoiceId = voice.voiceId
                            character.voiceDescription = voice.name
                            dismiss()
                        }) {
                            HStack {
                                VStack(alignment: .leading) {
                                    Text(voice.name)
                                        .font(.headline)
                                    if let category = voice.category {
                                        Text(category)
                                            .font(.caption)
                                            .foregroundColor(.secondary)
                                    }
                                }

                                Spacer()

                                if character.elevenLabsVoiceId == voice.voiceId {
                                    Image(systemName: "checkmark")
                                        .foregroundColor(.blue)
                                }
                            }
                        }
                    }
                }
            }
            .navigationTitle("Select Voice")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button("Done") {
                        dismiss()
                    }
                }
            }
            .task {
                await viewModel.fetchAvailableVoices()
            }
        }
    }
}

#Preview {
    CharacterCreateView(character: Character())
        .environmentObject(LibraryViewModel())
}
