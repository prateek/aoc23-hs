//
//  StoryCreatorApp.swift
//  StoryCreator
//
//  Main app entry point
//

import SwiftUI

@main
struct StoryCreatorApp: App {
    @StateObject private var libraryViewModel = LibraryViewModel()
    @StateObject private var apiConfig = APIConfiguration.shared

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(libraryViewModel)
                .environmentObject(apiConfig)
        }
    }
}
