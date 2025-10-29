//
//  ContentView.swift
//  StoryCreator
//
//  Main navigation view
//

import SwiftUI

struct ContentView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @EnvironmentObject var apiConfig: APIConfiguration
    @State private var selectedTab = 0
    @State private var showingSettings = false

    var body: some View {
        NavigationView {
            TabView(selection: $selectedTab) {
                LibraryView()
                    .tabItem {
                        Label("Library", systemImage: "books.vertical")
                    }
                    .tag(0)

                CharacterListView()
                    .tabItem {
                        Label("Characters", systemImage: "person.2")
                    }
                    .tag(1)

                CreateStoryView()
                    .tabItem {
                        Label("Create", systemImage: "plus.circle.fill")
                    }
                    .tag(2)
            }
            .navigationTitle(tabTitle)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: { showingSettings = true }) {
                        Image(systemName: "gear")
                    }
                }
            }
            .sheet(isPresented: $showingSettings) {
                SettingsView()
            }
        }
        .navigationViewStyle(StackNavigationViewStyle())
    }

    private var tabTitle: String {
        switch selectedTab {
        case 0: return "Story Library"
        case 1: return "Characters"
        case 2: return "Create Story"
        default: return "StoryCreator"
        }
    }
}

struct CreateStoryView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @State private var showingNewStory = false

    var body: some View {
        VStack(spacing: 20) {
            Image(systemName: "book.pages")
                .font(.system(size: 80))
                .foregroundColor(.blue)

            Text("Create Your Story")
                .font(.largeTitle)
                .bold()

            Text("Use AI to generate amazing stories for kids")
                .font(.subheadline)
                .foregroundColor(.secondary)
                .multilineTextAlignment(.center)
                .padding(.horizontal)

            Button(action: { showingNewStory = true }) {
                Label("New Story", systemImage: "plus.circle.fill")
                    .font(.headline)
                    .foregroundColor(.white)
                    .frame(maxWidth: .infinity)
                    .padding()
                    .background(Color.blue)
                    .cornerRadius(10)
            }
            .padding(.horizontal, 40)

            Spacer()
        }
        .padding()
        .sheet(isPresented: $showingNewStory) {
            StorySetupView()
        }
    }
}

#Preview {
    ContentView()
        .environmentObject(LibraryViewModel())
        .environmentObject(APIConfiguration.shared)
}
