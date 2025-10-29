//
//  APIConfiguration.swift
//  StoryCreator
//
//  Configuration for API keys and endpoints
//

import Foundation

class APIConfiguration: ObservableObject {
    @Published var openAIKey: String = ""
    @Published var geminiKey: String = ""
    @Published var elevenLabsKey: String = ""
    @Published var yotoEmail: String = ""
    @Published var yotoPassword: String = ""
    @Published var selectedLLMProvider: LLMProvider = .openAI

    enum LLMProvider: String, CaseIterable {
        case openAI = "OpenAI"
        case gemini = "Google Gemini"
    }

    static let shared = APIConfiguration()

    private init() {
        loadFromUserDefaults()
    }

    func saveToUserDefaults() {
        UserDefaults.standard.set(openAIKey, forKey: "openAIKey")
        UserDefaults.standard.set(geminiKey, forKey: "geminiKey")
        UserDefaults.standard.set(elevenLabsKey, forKey: "elevenLabsKey")
        UserDefaults.standard.set(yotoEmail, forKey: "yotoEmail")
        UserDefaults.standard.set(yotoPassword, forKey: "yotoPassword")
        UserDefaults.standard.set(selectedLLMProvider.rawValue, forKey: "selectedLLMProvider")
    }

    func loadFromUserDefaults() {
        openAIKey = UserDefaults.standard.string(forKey: "openAIKey") ?? ""
        geminiKey = UserDefaults.standard.string(forKey: "geminiKey") ?? ""
        elevenLabsKey = UserDefaults.standard.string(forKey: "elevenLabsKey") ?? ""
        yotoEmail = UserDefaults.standard.string(forKey: "yotoEmail") ?? ""
        yotoPassword = UserDefaults.standard.string(forKey: "yotoPassword") ?? ""
        if let providerString = UserDefaults.standard.string(forKey: "selectedLLMProvider"),
           let provider = LLMProvider(rawValue: providerString) {
            selectedLLMProvider = provider
        }
    }

    var isConfigured: Bool {
        let hasLLM = !openAIKey.isEmpty || !geminiKey.isEmpty
        let hasElevenLabs = !elevenLabsKey.isEmpty
        return hasLLM && hasElevenLabs
    }

    var hasYotoCredentials: Bool {
        return !yotoEmail.isEmpty && !yotoPassword.isEmpty
    }
}
