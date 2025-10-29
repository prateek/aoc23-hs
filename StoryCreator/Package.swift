// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "StoryCreator",
    platforms: [
        .iOS(.v16)
    ],
    products: [
        .library(
            name: "StoryCreator",
            targets: ["StoryCreator"]),
    ],
    targets: [
        .target(
            name: "StoryCreator",
            dependencies: []),
    ]
)
