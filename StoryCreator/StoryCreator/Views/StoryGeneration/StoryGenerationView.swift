//
//  StoryGenerationView.swift
//  StoryCreator
//
//  Story and audio generation view
//

import SwiftUI

struct StoryGenerationView: View {
    @EnvironmentObject var libraryViewModel: LibraryViewModel
    @Environment(\.dismiss) var dismiss
    @ObservedObject var viewModel: StoryViewModel
    @State private var currentStep = GenerationStep.sceneText
    @State private var navigateToDetail = false

    enum GenerationStep: String, CaseIterable {
        case sceneText = "Generating Scene Text"
        case fullStory = "Generating Full Story"
        case audio = "Generating Audio"
        case complete = "Complete"
    }

    var body: some View {
        NavigationView {
            VStack(spacing: 30) {
                // Progress header
                VStack(spacing: 12) {
                    if viewModel.isGenerating {
                        ProgressView(value: viewModel.generationProgress)
                            .progressViewStyle(.linear)
                            .padding(.horizontal)

                        Text(currentStep.rawValue)
                            .font(.headline)

                        Text("\(Int(viewModel.generationProgress * 100))%")
                            .font(.caption)
                            .foregroundColor(.secondary)
                    } else if viewModel.errorMessage != nil {
                        Image(systemName: "exclamationmark.triangle.fill")
                            .font(.system(size: 60))
                            .foregroundColor(.red)

                        Text("Generation Failed")
                            .font(.title2)
                            .bold()

                        Text(viewModel.errorMessage!)
                            .font(.subheadline)
                            .foregroundColor(.secondary)
                            .multilineTextAlignment(.center)
                            .padding()
                    } else {
                        Image(systemName: "checkmark.circle.fill")
                            .font(.system(size: 60))
                            .foregroundColor(.green)

                        Text("Story Generated!")
                            .font(.title2)
                            .bold()

                        Text("Your story is ready with audio")
                            .font(.subheadline)
                            .foregroundColor(.secondary)
                    }
                }
                .padding()

                // Generation steps
                VStack(alignment: .leading, spacing: 16) {
                    ForEach(GenerationStep.allCases, id: \.self) { step in
                        GenerationStepRow(
                            step: step,
                            isComplete: isStepComplete(step),
                            isCurrent: currentStep == step
                        )
                    }
                }
                .padding()

                Spacer()

                // Actions
                if !viewModel.isGenerating {
                    VStack(spacing: 12) {
                        if viewModel.errorMessage != nil {
                            Button(action: { Task { await startGeneration() } }) {
                                Label("Retry", systemImage: "arrow.clockwise")
                                    .font(.headline)
                                    .foregroundColor(.white)
                                    .frame(maxWidth: .infinity)
                                    .padding()
                                    .background(Color.blue)
                                    .cornerRadius(10)
                            }
                        } else if currentStep == .complete {
                            Button(action: {
                                libraryViewModel.updateStory(viewModel.story)
                                navigateToDetail = true
                            }) {
                                Label("View Story", systemImage: "book")
                                    .font(.headline)
                                    .foregroundColor(.white)
                                    .frame(maxWidth: .infinity)
                                    .padding()
                                    .background(Color.blue)
                                    .cornerRadius(10)
                            }
                        }

                        Button(action: { dismiss() }) {
                            Text(currentStep == .complete ? "Close" : "Cancel")
                                .font(.headline)
                                .frame(maxWidth: .infinity)
                                .padding()
                        }
                    }
                    .padding()
                }
            }
            .navigationTitle("Generating Story")
            .navigationBarTitleDisplayMode(.inline)
            .navigationDestination(isPresented: $navigateToDetail) {
                StoryDetailView(story: viewModel.story)
            }
            .task {
                if !viewModel.isGenerating && currentStep == .sceneText {
                    await startGeneration()
                }
            }
        }
    }

    private func startGeneration() async {
        await viewModel.generateCompleteStory()
        if viewModel.errorMessage == nil {
            currentStep = .complete
        }
    }

    private func isStepComplete(_ step: GenerationStep) -> Bool {
        let steps = GenerationStep.allCases
        guard let currentIndex = steps.firstIndex(of: currentStep),
              let stepIndex = steps.firstIndex(of: step) else {
            return false
        }
        return stepIndex < currentIndex || (!viewModel.isGenerating && currentStep == .complete)
    }
}

struct GenerationStepRow: View {
    let step: StoryGenerationView.GenerationStep
    let isComplete: Bool
    let isCurrent: Bool

    var body: some View {
        HStack(spacing: 16) {
            ZStack {
                Circle()
                    .fill(backgroundColor)
                    .frame(width: 40, height: 40)

                if isComplete {
                    Image(systemName: "checkmark")
                        .foregroundColor(.white)
                } else if isCurrent {
                    ProgressView()
                        .progressViewStyle(CircularProgressViewStyle(tint: .white))
                } else {
                    Circle()
                        .fill(Color.white)
                        .frame(width: 12, height: 12)
                }
            }

            Text(step.rawValue)
                .font(isCurrent ? .headline : .subheadline)
                .foregroundColor(isCurrent ? .primary : .secondary)

            Spacer()
        }
    }

    private var backgroundColor: Color {
        if isComplete {
            return .green
        } else if isCurrent {
            return .blue
        } else {
            return .gray.opacity(0.3)
        }
    }
}

#Preview {
    StoryGenerationView(viewModel: StoryViewModel(story: Story(title: "Test Story")))
        .environmentObject(LibraryViewModel())
}
