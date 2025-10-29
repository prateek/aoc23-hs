//
//  LibraryView.swift
//  StoryCreator
//
//  Story library view
//

import SwiftUI

struct LibraryView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @State private var showingFilterSheet = false

    var body: some View {
        VStack {
            if libraryViewModel.filteredStories.isEmpty {
                emptyStateView
            } else {
                storyListView
            }
        }
        .searchable(text: $libraryViewModel.searchText, prompt: "Search stories")
        .toolbar {
            ToolbarItem(placement: .navigationBarTrailing) {
                Button(action: { showingFilterSheet = true }) {
                    Image(systemName: "line.3.horizontal.decrease.circle")
                }
            }
        }
        .sheet(isPresented: $showingFilterSheet) {
            FilterView()
        }
    }

    private var emptyStateView: some View {
        VStack(spacing: 20) {
            Image(systemName: "book.closed")
                .font(.system(size: 60))
                .foregroundColor(.gray)

            Text("No Stories Yet")
                .font(.title2)
                .bold()

            Text("Create your first story to get started")
                .font(.subheadline)
                .foregroundColor(.secondary)
        }
        .padding()
    }

    private var storyListView: some View {
        ScrollView {
            LazyVStack(spacing: 16) {
                ForEach(libraryViewModel.filteredStories) { story in
                    NavigationLink(destination: StoryDetailView(story: story)) {
                        StoryCardView(story: story)
                    }
                    .buttonStyle(PlainButtonStyle())
                }
            }
            .padding()
        }
    }
}

struct FilterView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @Environment(\.dismiss) var dismiss

    var body: some View {
        NavigationView {
            Form {
                Section("Genre") {
                    Picker("Genre", selection: $libraryViewModel.selectedGenreFilter) {
                        Text("All Genres").tag(nil as StoryGenre?)
                        ForEach(StoryGenre.allCases, id: \.self) { genre in
                            Text(genre.rawValue).tag(genre as StoryGenre?)
                        }
                    }
                }

                Section("Age Range") {
                    Picker("Age", selection: $libraryViewModel.selectedAgeFilter) {
                        Text("All Ages").tag(nil as AgeRange?)
                        ForEach(AgeRange.allCases, id: \.self) { age in
                            Text(age.rawValue).tag(age as AgeRange?)
                        }
                    }
                }

                Section {
                    Button("Clear Filters") {
                        libraryViewModel.clearFilters()
                    }
                }
            }
            .navigationTitle("Filter Stories")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button("Done") {
                        dismiss()
                    }
                }
            }
        }
    }
}

#Preview {
    NavigationView {
        LibraryView()
            .environmentObject(LibraryViewModel())
    }
}
