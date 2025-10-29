//
//  SceneEditView.swift
//  StoryCreator
//
//  Scene editing view
//

import SwiftUI

struct SceneEditView: View {
    @Environment(\.dismiss) var dismiss
    @State var scene: StoryScene
    @State private var newPlotPoint = ""

    var onSave: ((StoryScene) -> Void)?

    var body: some View {
        NavigationView {
            Form {
                Section("Scene Information") {
                    TextField("Scene Title", text: $scene.title)
                    TextField("Location", text: $scene.location)
                    TextField("Emotion/Tone", text: $scene.emotion)
                }

                Section("Description") {
                    TextEditor(text: $scene.description)
                        .frame(minHeight: 100)
                }

                Section("Plot Points") {
                    ForEach(Array(scene.plotPoints.enumerated()), id: \.offset) { index, point in
                        HStack {
                            Text("• \(point)")
                            Spacer()
                            Button(action: {
                                scene.plotPoints.remove(at: index)
                            }) {
                                Image(systemName: "trash")
                                    .foregroundColor(.red)
                            }
                        }
                    }

                    HStack {
                        TextField("Add plot point", text: $newPlotPoint)
                        Button(action: addPlotPoint) {
                            Image(systemName: "plus.circle.fill")
                                .foregroundColor(.blue)
                        }
                        .disabled(newPlotPoint.isEmpty)
                    }
                }

                Section("Notes") {
                    TextEditor(text: $scene.notes)
                        .frame(minHeight: 80)
                }
            }
            .navigationTitle(scene.title.isEmpty ? "New Scene" : scene.title)
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarLeading) {
                    Button("Cancel") {
                        dismiss()
                    }
                }

                ToolbarItem(placement: .navigationBarTrailing) {
                    Button("Save") {
                        saveScene()
                    }
                    .disabled(scene.title.isEmpty)
                }
            }
        }
    }

    private func addPlotPoint() {
        guard !newPlotPoint.isEmpty else { return }
        scene.plotPoints.append(newPlotPoint)
        newPlotPoint = ""
    }

    private func saveScene() {
        scene.updatedAt = Date()
        onSave?(scene)
        dismiss()
    }
}

#Preview {
    SceneEditView(scene: StoryScene(title: "Opening Scene"))
}
