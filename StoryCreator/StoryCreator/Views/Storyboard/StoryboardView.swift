//
//  StoryboardView.swift
//  StoryCreator
//
//  Visual storyboard editor view
//

import SwiftUI

struct StoryboardView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @Environment(\.dismiss) var dismiss
    @StateObject private var viewModel: StoryViewModel
    @State private var selectedPlotStructure: PlotStructure = .threeAct
    @State private var showingAddScene = false
    @State private var editingScene: StoryScene?
    @State private var showingGenerationView = false

    init(story: Story, selectedCharacters: [Character]) {
        _viewModel = StateObject(wrappedValue: StoryViewModel(story: story))
        viewModel.wrappedValue.loadCharacters(selectedCharacters)
    }

    var body: some View {
        VStack(spacing: 0) {
            // Header
            headerView

            // Scenes
            if viewModel.story.scenes.isEmpty {
                emptyStateView
            } else {
                sceneListView
            }

            // Action buttons
            bottomBar
        }
        .navigationTitle("Storyboard")
        .navigationBarTitleDisplayMode(.inline)
        .sheet(isPresented: $showingAddScene) {
            SceneEditView(scene: StoryScene(orderIndex: viewModel.story.scenes.count))
                .onDisappear {
                    // Scene will be added by SceneEditView
                }
        }
        .sheet(item: $editingScene) { scene in
            SceneEditView(scene: scene)
        }
        .fullScreenCover(isPresented: $showingGenerationView) {
            StoryGenerationView(viewModel: viewModel)
        }
    }

    private var headerView: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text(viewModel.story.title)
                .font(.title2)
                .bold()

            HStack {
                Text(viewModel.story.genre.rawValue)
                    .font(.caption)
                    .foregroundColor(.white)
                    .padding(.horizontal, 8)
                    .padding(.vertical, 4)
                    .background(Color.blue)
                    .cornerRadius(4)

                Text("\(viewModel.story.scenes.count) scenes")
                    .font(.caption)
                    .foregroundColor(.secondary)

                Spacer()

                Menu {
                    ForEach(PlotStructure.allCases, id: \.self) { structure in
                        Button(structure.rawValue) {
                            applyPlotStructure(structure)
                        }
                    }
                } label: {
                    Label("Templates", systemImage: "doc.text")
                        .font(.caption)
                }
            }
        }
        .padding()
        .background(Color(.systemGray6))
    }

    private var emptyStateView: some View {
        VStack(spacing: 20) {
            Image(systemName: "rectangle.3.group")
                .font(.system(size: 60))
                .foregroundColor(.gray)

            Text("No Scenes Yet")
                .font(.title2)
                .bold()

            Text("Add scenes to build your story structure")
                .font(.subheadline)
                .foregroundColor(.secondary)
                .multilineTextAlignment(.center)

            Button(action: { showingAddScene = true }) {
                Label("Add First Scene", systemImage: "plus.circle.fill")
                    .font(.headline)
                    .foregroundColor(.white)
                    .padding()
                    .background(Color.blue)
                    .cornerRadius(10)
            }

            Text("Or use a template:")
                .font(.caption)
                .foregroundColor(.secondary)

            Menu {
                ForEach(PlotStructure.allCases.filter { $0 != .custom }, id: \.self) { structure in
                    Button(structure.rawValue) {
                        applyPlotStructure(structure)
                    }
                }
            } label: {
                Label("Choose Template", systemImage: "doc.text")
                    .font(.subheadline)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private var sceneListView: some View {
        ScrollView {
            LazyVStack(spacing: 12) {
                ForEach(Array(viewModel.story.scenes.enumerated()), id: \.element.id) { index, scene in
                    SceneCardView(
                        scene: scene,
                        index: index,
                        onEdit: { editingScene = scene },
                        onDelete: { viewModel.removeScene(at: index) }
                    )
                }
            }
            .padding()
        }
    }

    private var bottomBar: some View {
        HStack(spacing: 16) {
            Button(action: { showingAddScene = true }) {
                Label("Add Scene", systemImage: "plus")
            }
            .buttonStyle(.bordered)

            Spacer()

            Button(action: {
                libraryViewModel.addStory(viewModel.story)
                showingGenerationView = true
            }) {
                Label("Generate Story", systemImage: "wand.and.stars")
                    .foregroundColor(.white)
                    .padding(.horizontal, 20)
                    .padding(.vertical, 10)
                    .background(Color.blue)
                    .cornerRadius(8)
            }
            .disabled(viewModel.story.scenes.isEmpty)
        }
        .padding()
        .background(Color(.systemGray6))
    }

    private func applyPlotStructure(_ structure: PlotStructure) {
        let sceneTemplates = structure.defaultScenes
        viewModel.story.scenes = sceneTemplates.enumerated().map { index, title in
            StoryScene(
                orderIndex: index,
                title: title,
                characterIds: viewModel.story.characterIds
            )
        }
    }
}

struct SceneCardView: View {
    let scene: StoryScene
    let index: Int
    let onEdit: () -> Void
    let onDelete: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
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

                Menu {
                    Button(action: onEdit) {
                        Label("Edit", systemImage: "pencil")
                    }
                    Button(role: .destructive, action: onDelete) {
                        Label("Delete", systemImage: "trash")
                    }
                } label: {
                    Image(systemName: "ellipsis.circle")
                        .foregroundColor(.blue)
                }
            }

            if !scene.description.isEmpty {
                Text(scene.description)
                    .font(.subheadline)
                    .foregroundColor(.secondary)
                    .lineLimit(3)
            }

            if !scene.location.isEmpty {
                Label(scene.location, systemImage: "location")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }

            if !scene.plotPoints.isEmpty {
                VStack(alignment: .leading, spacing: 4) {
                    ForEach(scene.plotPoints, id: \.self) { point in
                        HStack(alignment: .top) {
                            Text("•")
                            Text(point)
                                .font(.caption)
                        }
                        .foregroundColor(.secondary)
                    }
                }
            }

            if scene.generatedText != nil {
                Label("Text generated", systemImage: "checkmark.circle.fill")
                    .font(.caption)
                    .foregroundColor(.green)
            }
        }
        .padding()
        .background(Color(.systemGray6))
        .cornerRadius(12)
    }
}

#Preview {
    NavigationView {
        StoryboardView(
            story: Story(title: "Test Story", genre: .adventure),
            selectedCharacters: []
        )
        .environmentObject(LibraryViewModel())
    }
}
