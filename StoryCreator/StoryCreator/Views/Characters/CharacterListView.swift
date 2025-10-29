//
//  CharacterListView.swift
//  StoryCreator
//
//  Character library view
//

import SwiftUI

struct CharacterListView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @State private var showingNewCharacter = false
    @State private var selectedCharacter: Character?

    var body: some View {
        VStack {
            if libraryViewModel.characters.isEmpty {
                emptyStateView
            } else {
                characterListView
            }
        }
        .toolbar {
            ToolbarItem(placement: .navigationBarTrailing) {
                Button(action: { showingNewCharacter = true }) {
                    Image(systemName: "plus")
                }
            }
        }
        .sheet(isPresented: $showingNewCharacter) {
            CharacterCreateView(character: Character())
        }
        .sheet(item: $selectedCharacter) { character in
            CharacterDetailView(character: character)
        }
    }

    private var emptyStateView: some View {
        VStack(spacing: 20) {
            Image(systemName: "person.3")
                .font(.system(size: 60))
                .foregroundColor(.gray)

            Text("No Characters Yet")
                .font(.title2)
                .bold()

            Text("Create characters to use in your stories")
                .font(.subheadline)
                .foregroundColor(.secondary)

            Button(action: { showingNewCharacter = true }) {
                Label("Create Character", systemImage: "plus.circle.fill")
                    .font(.headline)
                    .foregroundColor(.white)
                    .padding()
                    .background(Color.blue)
                    .cornerRadius(10)
            }
        }
        .padding()
    }

    private var characterListView: some View {
        List {
            ForEach(libraryViewModel.characters) { character in
                CharacterRowView(character: character)
                    .contentShape(Rectangle())
                    .onTapGesture {
                        selectedCharacter = character
                    }
            }
            .onDelete(perform: deleteCharacters)
        }
    }

    private func deleteCharacters(at offsets: IndexSet) {
        for index in offsets {
            let character = libraryViewModel.characters[index]
            libraryViewModel.deleteCharacter(character)
        }
    }
}

struct CharacterRowView: View {
    let character: Character

    var body: some View {
        HStack(spacing: 12) {
            // Character icon
            ZStack {
                Circle()
                    .fill(Color.blue.opacity(0.2))
                    .frame(width: 50, height: 50)

                Text(character.name.prefix(1).uppercased())
                    .font(.title3)
                    .bold()
                    .foregroundColor(.blue)
            }

            VStack(alignment: .leading, spacing: 4) {
                Text(character.name)
                    .font(.headline)

                HStack {
                    Text(character.role.rawValue)
                        .font(.caption)
                        .foregroundColor(.white)
                        .padding(.horizontal, 6)
                        .padding(.vertical, 2)
                        .background(roleColor(character.role))
                        .cornerRadius(4)

                    if !character.age.isEmpty {
                        Text(character.age)
                            .font(.caption)
                            .foregroundColor(.secondary)
                    }
                }

                if !character.personality.isEmpty {
                    Text(character.personality)
                        .font(.caption)
                        .foregroundColor(.secondary)
                        .lineLimit(1)
                }
            }

            Spacer()

            if character.elevenLabsVoiceId != nil {
                Image(systemName: "speaker.wave.2")
                    .foregroundColor(.green)
            }
        }
        .padding(.vertical, 4)
    }

    private func roleColor(_ role: CharacterRole) -> Color {
        switch role {
        case .protagonist: return .blue
        case .antagonist: return .red
        case .supporting: return .green
        case .sidekick: return .orange
        case .mentor: return .purple
        case .other: return .gray
        }
    }
}

#Preview {
    NavigationView {
        CharacterListView()
            .environmentObject(LibraryViewModel())
    }
}
