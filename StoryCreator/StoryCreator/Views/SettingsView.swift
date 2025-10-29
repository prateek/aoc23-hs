//
//  SettingsView.swift
//  StoryCreator
//
//  Settings and API configuration view
//

import SwiftUI

struct SettingsView: View {
    @EnvironmentObject var apiConfig: APIConfiguration
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @Environment(\.dismiss) var dismiss

    @State private var showingClearDataAlert = false

    var body: some View {
        NavigationView {
            Form {
                Section {
                    Text("Configure your API keys to enable story generation and audio features")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }

                Section("LLM Provider") {
                    Picker("Provider", selection: $apiConfig.selectedLLMProvider) {
                        ForEach(APIConfiguration.LLMProvider.allCases, id: \.self) { provider in
                            Text(provider.rawValue).tag(provider)
                        }
                    }

                    if apiConfig.selectedLLMProvider == .openAI {
                        SecureField("OpenAI API Key", text: $apiConfig.openAIKey)
                            .textContentType(.password)
                    } else {
                        SecureField("Gemini API Key", text: $apiConfig.geminiKey)
                            .textContentType(.password)
                    }

                    Link("Get API Key", destination: apiKeyURL)
                        .font(.caption)
                }

                Section("ElevenLabs") {
                    SecureField("ElevenLabs API Key", text: $apiConfig.elevenLabsKey)
                        .textContentType(.password)

                    Link("Get ElevenLabs Key", destination: URL(string: "https://elevenlabs.io/api")!)
                        .font(.caption)
                }

                Section("Yoto Integration") {
                    TextField("Email", text: $apiConfig.yotoEmail)
                        .textContentType(.emailAddress)
                        .autocapitalization(.none)
                        .keyboardType(.emailAddress)

                    SecureField("Password", text: $apiConfig.yotoPassword)
                        .textContentType(.password)

                    Link("Yoto Account", destination: URL(string: "https://yotoplay.com")!)
                        .font(.caption)
                }

                Section("Configuration Status") {
                    StatusRow(
                        label: "LLM Configured",
                        isConfigured: !apiConfig.openAIKey.isEmpty || !apiConfig.geminiKey.isEmpty
                    )

                    StatusRow(
                        label: "ElevenLabs Configured",
                        isConfigured: !apiConfig.elevenLabsKey.isEmpty
                    )

                    StatusRow(
                        label: "Yoto Configured",
                        isConfigured: apiConfig.hasYotoCredentials
                    )
                }

                Section("Library Statistics") {
                    StatRow(label: "Total Stories", value: "\(libraryViewModel.getTotalStories())")
                    StatRow(label: "Total Characters", value: "\(libraryViewModel.getTotalCharacters())")
                    StatRow(label: "Stories with Audio", value: "\(libraryViewModel.getStoriesWithAudio())")
                    StatRow(label: "Stories on Yoto", value: "\(libraryViewModel.getStoriesOnYoto())")
                }

                Section("Data Management") {
                    Button(role: .destructive, action: { showingClearDataAlert = true }) {
                        Text("Clear All Data")
                    }
                }

                Section("About") {
                    HStack {
                        Text("Version")
                        Spacer()
                        Text("1.0.0")
                            .foregroundColor(.secondary)
                    }

                    Link("Privacy Policy", destination: URL(string: "https://example.com/privacy")!)
                    Link("Terms of Service", destination: URL(string: "https://example.com/terms")!)
                }
            }
            .navigationTitle("Settings")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button("Done") {
                        apiConfig.saveToUserDefaults()
                        dismiss()
                    }
                }
            }
            .alert("Clear All Data", isPresented: $showingClearDataAlert) {
                Button("Cancel", role: .cancel) { }
                Button("Clear", role: .destructive) {
                    clearAllData()
                }
            } message: {
                Text("This will delete all stories, characters, and audio files. This action cannot be undone.")
            }
        }
    }

    private var apiKeyURL: URL {
        if apiConfig.selectedLLMProvider == .openAI {
            return URL(string: "https://platform.openai.com/api-keys")!
        } else {
            return URL(string: "https://makersuite.google.com/app/apikey")!
        }
    }

    private func clearAllData() {
        // Delete audio files
        for story in libraryViewModel.stories {
            for audioTrack in story.audioTracks {
                if let localPath = audioTrack.localAudioPath {
                    try? FileManager.default.removeItem(atPath: localPath)
                }
            }
        }

        // Clear data
        UserDefaults.standard.removeObject(forKey: "SavedStories")
        UserDefaults.standard.removeObject(forKey: "SavedCharacters")

        // Reload
        libraryViewModel.stories.removeAll()
        libraryViewModel.characters.removeAll()
        libraryViewModel.filterStories()
    }
}

struct StatusRow: View {
    let label: String
    let isConfigured: Bool

    var body: some View {
        HStack {
            Text(label)
            Spacer()
            Image(systemName: isConfigured ? "checkmark.circle.fill" : "xmark.circle.fill")
                .foregroundColor(isConfigured ? .green : .red)
        }
    }
}

struct StatRow: View {
    let label: String
    let value: String

    var body: some View {
        HStack {
            Text(label)
            Spacer()
            Text(value)
                .foregroundColor(.secondary)
        }
    }
}

#Preview {
    SettingsView()
        .environmentObject(APIConfiguration.shared)
        .environmentObject(LibraryViewModel())
}
