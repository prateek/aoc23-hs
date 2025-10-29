# StoryCreator - Quick Start Guide

Get up and running in 5 minutes!

## Prerequisites

Before you begin, you'll need:
- [ ] An iPhone or iPad running iOS 16+
- [ ] Xcode 15+ (for building the app)
- [ ] OpenAI API key OR Google Gemini API key
- [ ] ElevenLabs API key
- [ ] (Optional) Yoto account for Yoto integration

## Step 1: Get Your API Keys (5 minutes)

### OpenAI (Recommended for Best Quality)
1. Go to https://platform.openai.com/api-keys
2. Sign up or log in
3. Click "Create new secret key"
4. Copy and save your key
5. Add $5-10 credit to your account

**Cost**: ~$0.02 per short story

### OR Google Gemini (Free Option)
1. Go to https://makersuite.google.com/app/apikey
2. Sign in with Google
3. Click "Create API key"
4. Copy and save your key

**Cost**: FREE (60 requests/min limit)

### ElevenLabs (Required for Audio)
1. Go to https://elevenlabs.io
2. Sign up for an account
3. Go to Profile → API Keys
4. Generate and copy your key

**Cost**: $5 for ~10-15 stories (or 10,000 chars/month free)

## Step 2: Build the App (2 minutes)

```bash
# Clone and open
cd StoryCreator
open StoryCreator.xcodeproj

# In Xcode:
# 1. Select your development team
# 2. Choose your device/simulator
# 3. Press ⌘+R to build and run
```

## Step 3: Configure the App (1 minute)

1. Open the app
2. Tap the gear icon ⚙️ (top right)
3. Select your LLM provider (OpenAI or Gemini)
4. Paste your API keys
5. Tap "Done"

✅ Green checkmarks = you're ready!

## Step 4: Create Your First Story (10 minutes)

### Create a Character (2 min)
1. Tap **Characters** tab
2. Tap **+** button
3. Fill in:
   - Name: "Luna the Fox"
   - Age: "3 years old"
   - Role: "Protagonist"
   - Personality: "Curious, brave, and kind"
4. Tap **Select Voice** → Choose a voice
5. Tap **Save**

### Start Your Story (2 min)
1. Tap **Create** tab
2. Tap **New Story**
3. Fill in:
   - Title: "Luna's Forest Adventure"
   - Genre: "Adventure"
   - Age: "Preschool (2-4 years)"
   - Synopsis: "Luna explores a magical forest"
   - Theme: "Courage and friendship"
4. Select "Luna the Fox"
5. Tap **Next**

### Build Your Storyboard (3 min)
1. Tap **Three-Act Structure** template
2. Edit Scene 1 (Setup):
   - Title: "Meeting Luna"
   - Description: "Luna wakes up and decides to explore"
   - Plot points: "Luna sees a mysterious light in the forest"
3. Repeat for Scenes 2-3 or keep defaults
4. Tap **Generate Story**

### Generate! (3 min)
Watch as the app:
1. ✨ Writes your story (30 seconds)
2. 🎙️ Creates audio narration (2 minutes)
3. ✅ Saves to your library

### Listen & Share
1. Go to **Library** tab
2. Tap your story
3. Tap ▶️ to listen
4. (Optional) Upload to Yoto!

## Next Steps

### Improve Your Stories
- Use the "Enhance with AI" feature for character descriptions
- Try different plot structures (Hero's Journey, etc.)
- Create multiple characters with relationships
- Experiment with different genres and age ranges

### Upload to Yoto (Optional)
1. Make sure you have:
   - A Yoto player
   - Yoto MYO (Make Your Own) cards
   - Yoto account credentials
2. Enter Yoto email/password in Settings
3. In your story, tap **Upload to Yoto**
4. Link the playlist to your MYO card in the Yoto app

### Save Money
- Start with **Gemini** (free tier) for testing
- Use **shorter stories** (3-5 scenes) first
- **Reuse characters** across multiple stories
- **Preview text** before generating audio

## Troubleshooting

### "Missing API Key" Error
→ Go to Settings and verify your keys are entered correctly

### Audio Generation is Slow
→ Normal! Audio takes 30-60 seconds per scene

### App Won't Build
→ Make sure you're using Xcode 15+ and have selected a development team

### "Request Failed" Error
→ Check your API key has sufficient credits/quota

## Tips for Great Stories

1. **Keep it simple**: 3-5 scenes work best for young kids
2. **Use repetition**: Kids love repeated phrases ("And then...")
3. **Short scenes**: Aim for 100-200 words per scene
4. **Clear emotions**: Specify the mood for each scene
5. **Preview first**: Review generated text before creating audio

## Example Story Flow

```
1. Create Character: "Max the Dragon" (Protagonist, age 4)
2. Set Story: Fantasy genre, Preschool age, "Max learns to fly"
3. Build Scenes:
   - Setup: "Max tries to fly but falls"
   - Confrontation: "His friend teaches him technique"
   - Resolution: "Max successfully flies for the first time"
4. Generate: Wait 3-5 minutes
5. Listen: Play audio tracks
6. Share: Upload to Yoto or share with family
```

## Cost Calculator

### Per Story (approx.)
- **Text Generation**: $0.01 - $0.03 (OpenAI) or FREE (Gemini)
- **Audio Generation**: $0.30 - $0.50 (ElevenLabs)
- **Total**: ~$0.35 per story

### Budget Recommendations
- **Casual use**: $10/month (20-30 stories)
- **Regular use**: $25/month (50-75 stories)
- **Heavy use**: $50/month (100+ stories)

Use Gemini free tier + ElevenLabs free tier to create ~30 stories/month FREE!

## Support & Community

- 📖 Full Documentation: See README.md
- 🐛 Report Issues: GitHub Issues
- 💡 Feature Ideas: GitHub Discussions
- 📧 Email Support: [your-email]

## What's Next?

Once you're comfortable with the basics:
1. Explore different plot structures
2. Create a character library
3. Try longer, more complex stories
4. Experiment with different voice actors
5. Build story series with recurring characters

---

**Happy storytelling! 📚✨**

Your kids will love the personalized stories you create!
