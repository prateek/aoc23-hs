# StoryCreator - AI-Powered Story Generation for Kids

An iOS app that helps parents create engaging, personalized stories for their children using AI-powered text and audio generation. Inspired by Novelcrafter, StoryCreator combines intuitive storyboarding, character creation, and seamless integration with Yoto players.

## Features

### Core Features
- **Character Creation**: Create detailed characters with personality traits, backstories, relationships, and custom voice assignments
- **Visual Storyboarding**: Plan your story structure using proven plot templates (Three-Act, Hero's Journey, Kishōtenketsu)
- **AI Story Generation**: Generate engaging, age-appropriate stories using OpenAI GPT or Google Gemini
- **Audio Generation**: Convert stories to professional narration using ElevenLabs text-to-speech
- **Yoto Integration**: Upload your stories directly to Yoto players for offline listening

### Novelcrafter-Inspired Features
- **Story Library**: Organize and manage all your created stories
- **Character Codex**: Maintain a library of reusable characters across stories
- **Scene-by-Scene Planning**: Break down stories into manageable scenes with plot points
- **Relationship Mapping**: Define character relationships and connections
- **Genre & Age Targeting**: Customize content for specific age ranges and genres

## Screenshots

[Screenshots would go here in production]

## Requirements

- iOS 16.0 or later
- iPhone or iPad
- Active internet connection for AI generation
- API keys for:
  - OpenAI (GPT-4o-mini) OR Google Gemini
  - ElevenLabs
  - Yoto account (optional, for Yoto integration)

## Installation

### Option 1: Build from Source

1. Clone this repository:
```bash
git clone <repository-url>
cd StoryCreator
```

2. Open the project in Xcode:
```bash
open StoryCreator.xcodeproj
```

3. Select your development team in the project settings

4. Build and run on your device or simulator (⌘+R)

### Option 2: TestFlight (Coming Soon)

TestFlight distribution will be available soon.

## Setup & Configuration

### 1. Get API Keys

#### OpenAI API Key
1. Visit [OpenAI Platform](https://platform.openai.com/api-keys)
2. Create an account or sign in
3. Generate a new API key
4. Copy the key (it won't be shown again)

**Cost Estimate**: $0.01 - $0.05 per story (using GPT-4o-mini)

#### Google Gemini API Key (Alternative)
1. Visit [Google AI Studio](https://makersuite.google.com/app/apikey)
2. Sign in with your Google account
3. Create a new API key
4. Copy the key

**Cost**: Free tier available (60 requests per minute)

#### ElevenLabs API Key
1. Visit [ElevenLabs](https://elevenlabs.io/api)
2. Create an account
3. Go to Profile Settings → API Keys
4. Generate a new API key
5. Copy the key

**Cost Estimate**: $5 for ~50,000 characters (~10-15 short stories)
**Free Tier**: 10,000 characters per month

#### Yoto Account (Optional)
1. Visit [Yoto Play](https://yotoplay.com)
2. Create an account or sign in
3. Purchase Yoto MYO (Make Your Own) cards
4. Use your Yoto email and password in the app

### 2. Configure the App

1. Open StoryCreator on your device
2. Tap the gear icon (⚙️) in the top right
3. Enter your API keys:
   - Select your preferred LLM provider (OpenAI or Gemini)
   - Enter the corresponding API key
   - Enter your ElevenLabs API key
   - (Optional) Enter your Yoto credentials
4. Tap "Done" to save

Configuration status indicators will show green checkmarks when properly configured.

## Usage Guide

### Creating Your First Story

#### Step 1: Create Characters

1. Tap the **Characters** tab
2. Tap the **+** button
3. Fill in character details:
   - Name, age, and role (protagonist, antagonist, etc.)
   - Physical appearance
   - Personality traits
   - Backstory
4. Tap **Select Voice** to choose an ElevenLabs voice
5. Tap **Save**

**Tip**: Use the "Enhance with AI" button to automatically improve character descriptions!

#### Step 2: Setup Your Story

1. Tap the **Create** tab
2. Tap **New Story**
3. Fill in story details:
   - Title
   - Genre (Adventure, Fantasy, Mystery, etc.)
   - Target age range
   - Synopsis
   - Theme and setting
4. Select characters to include
5. Tap **Next**

#### Step 3: Build Your Storyboard

1. Choose a plot structure template or create custom scenes:
   - **Three-Act Structure**: Setup, Confrontation, Resolution
   - **Hero's Journey**: Classic monomyth structure
   - **Kishōtenketsu**: Japanese 4-act structure
   - **Five-Part Story Arc**: Traditional story arc
2. For each scene, add:
   - Scene title
   - Location
   - Description
   - Plot points
   - Characters involved
   - Emotional tone
3. Tap **Generate Story** when ready

#### Step 4: Generate Content

The app will automatically:
1. ✨ Generate text for each scene using your chosen LLM
2. 📖 Create a cohesive full story
3. 🎙️ Generate audio narration with ElevenLabs
4. ✅ Save everything to your library

This process typically takes 2-5 minutes depending on story length.

#### Step 5: Review & Upload

1. Review your generated story in the Library
2. Listen to audio tracks
3. Make any desired edits
4. Tap **Upload to Yoto** to send to your Yoto player

### Yoto Integration

#### Upload Limits
- **Max tracks per playlist**: 100
- **Max file size per track**: 100 MB
- **Max duration per track**: 60 minutes
- **Max total playlist duration**: 5 hours
- **Max total playlist size**: 500 MB

#### Linking to MYO Cards

After uploading:
1. Open the Yoto app on your phone
2. Go to **My Library** → **Make Your Own**
3. Find your story playlist
4. Link it to a physical Yoto MYO card
5. Insert the card into your Yoto player to play

## Project Structure

```
StoryCreator/
├── StoryCreator/
│   ├── StoryCreatorApp.swift          # App entry point
│   ├── Models/                         # Data models
│   │   ├── Character.swift
│   │   ├── Story.swift
│   │   ├── StoryScene.swift
│   │   ├── AudioTrack.swift
│   │   └── YotoPlaylist.swift
│   ├── Services/                       # API integrations
│   │   ├── APIConfiguration.swift
│   │   ├── LLMService.swift           # OpenAI/Gemini
│   │   ├── ElevenLabsService.swift
│   │   └── YotoService.swift
│   ├── ViewModels/                     # Business logic
│   │   ├── CharacterViewModel.swift
│   │   ├── StoryViewModel.swift
│   │   └── LibraryViewModel.swift
│   └── Views/                          # UI components
│       ├── ContentView.swift
│       ├── Characters/
│       ├── Storyboard/
│       ├── StoryGeneration/
│       ├── Library/
│       ├── Yoto/
│       └── SettingsView.swift
└── README.md
```

## Architecture

### Design Patterns
- **MVVM (Model-View-ViewModel)**: Clean separation of concerns
- **Observable Objects**: SwiftUI state management
- **Async/Await**: Modern concurrency for API calls
- **Service Layer**: Abstracted API integrations

### Data Flow
1. User creates story in Views
2. ViewModels manage business logic
3. Services handle API communication
4. Models represent data structures
5. UserDefaults for local persistence
6. FileManager for audio storage

## API Reference

### OpenAI Integration
- **Model**: GPT-4o-mini
- **Endpoint**: `/v1/chat/completions`
- **Temperature**: 0.8 (creative output)

### Gemini Integration
- **Model**: gemini-pro
- **Endpoint**: `/v1beta/models/gemini-pro:generateContent`
- **Temperature**: 0.8

### ElevenLabs Integration
- **Model**: eleven_multilingual_v2
- **Endpoint**: `/v1/text-to-speech/{voice_id}`
- **Settings**: Stability: 0.5, Similarity: 0.75

### Yoto API Integration
- **Base URL**: `https://api.yotoplay.com/v1`
- **Authentication**: Bearer token (email/password login)
- **Endpoints**:
  - POST `/auth/login`
  - POST `/playlists`
  - POST `/playlists/{id}/tracks`

## Tips & Best Practices

### Creating Great Stories
1. **Keep it simple**: Shorter stories work better for young children
2. **Use vivid characters**: Well-defined characters make stories memorable
3. **Include repetition**: Kids love predictable patterns and repeated phrases
4. **Age-appropriate themes**: Match complexity to target age range
5. **Test voices**: Preview ElevenLabs voices before assigning to characters

### Optimizing Costs
1. **Start with shorter stories**: Test with 3-5 scenes before creating longer content
2. **Reuse characters**: Build a character library to speed up creation
3. **Use Gemini free tier**: For experimentation and testing
4. **Preview before generating audio**: Review text before creating expensive audio

### Performance Tips
1. **Generate scenes individually**: For better control and faster iteration
2. **Save frequently**: Changes are auto-saved, but verify important work
3. **Download audio**: Audio files are stored locally after generation
4. **Manage storage**: Delete old stories to free up space

## Troubleshooting

### Common Issues

#### "Missing API Key" Error
- **Solution**: Go to Settings and enter your API keys
- Verify keys are copied correctly without extra spaces

#### Audio Generation Fails
- **Check**: ElevenLabs API key is valid
- **Verify**: You have remaining characters in your quota
- **Try**: A shorter text snippet first

#### Yoto Upload Fails
- **Check**: Audio files are under 100MB each
- **Verify**: Total playlist is under 5 hours
- **Ensure**: You have fewer than 100 tracks
- **Confirm**: Yoto credentials are correct

#### App Crashes During Generation
- **Try**: Restart the app
- **Check**: Device has adequate storage space
- **Reduce**: Story complexity (fewer scenes)

### Getting Help
- Check API provider status pages
- Review API key permissions
- Verify internet connection
- Contact support (see below)

## Privacy & Safety

### Data Storage
- **Local Storage**: Stories and characters stored on device only
- **API Keys**: Stored securely in UserDefaults
- **Audio Files**: Saved to device Documents directory
- **No Cloud Sync**: All data remains on your device

### Content Safety
- AI-generated content is designed for children
- Always review stories before sharing with kids
- Report any inappropriate content to AI providers
- Consider using content filtering if needed

### API Usage
- API keys are transmitted securely over HTTPS
- Keys are never shared or logged
- You maintain full control of your API accounts

## Roadmap

### Planned Features
- [ ] iCloud sync for stories and characters
- [ ] Image generation for story illustrations
- [ ] Multiple narrators per story
- [ ] Story templates library
- [ ] Export to PDF/eBook formats
- [ ] Collaborative story editing
- [ ] Story analytics and insights
- [ ] Custom voice cloning support
- [ ] Spotify/Apple Podcasts export

### Community Requests
Submit feature requests via GitHub Issues!

## Contributing

Contributions are welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## License

[Specify your license here]

## Support

For questions, issues, or feedback:
- 📧 Email: [your-email]
- 🐛 Issues: GitHub Issues
- 💬 Discussions: GitHub Discussions

## Acknowledgments

- **Novelcrafter**: Inspiration for storyboarding features
- **OpenAI**: GPT-4 language models
- **Google**: Gemini AI
- **ElevenLabs**: High-quality text-to-speech
- **Yoto**: Kids' audio player integration

## Disclaimer

This app requires paid API services. You are responsible for:
- Managing your API usage and costs
- Complying with API provider terms of service
- Reviewing generated content for appropriateness
- Yoto account and hardware purchases

API costs vary based on usage. Monitor your usage on provider dashboards.

---

**Made with ❤️ for parents who love storytelling**
