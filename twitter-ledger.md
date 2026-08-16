# 🐦 Anahata Twitter / X Media Ledger: Operation Deep Strike

This ledger tracks automated tweets, thread engagement, impressions, and developer interactions for the official **@AnahataASI** Twitter/X channel.

---

## 🔑 API Subscription & Key Configuration

### 1. How to Subscribe to the Twitter / X API:
1. Go to the [Twitter Developer Portal](https://developer.x.com/en/portal/dashboard).
2. Sign in with the **@AnahataASI** account.
3. Under **Projects & Apps**, create a new Project and App (e.g., `Anahata-ASI-Autonomous`).
4. Set User Authentication Settings:
   - **App Permissions**: `Read and Write and Direct message`
   - **Type of App**: `Web App, Automated App or Bot`
5. Generate the following credentials from the **Keys and Tokens** tab:
   - `API Key` (Consumer Key)
   - `API Key Secret` (Consumer Secret)
   - `Bearer Token`
   - `Access Token`
   - `Access Token Secret`

### 2. Key Storage Location:
Save the JSON configuration file to your user home directory at:
```bash
~/.anahata/twitter_api_keys.json
```

**JSON Structure:**
```json
{
  "apiKey": "YOUR_API_KEY",
  "apiSecret": "YOUR_API_SECRET",
  "bearerToken": "YOUR_BEARER_TOKEN",
  "accessToken": "YOUR_ACCESS_TOKEN",
  "accessTokenSecret": "YOUR_ACCESS_TOKEN_SECRET"
}
```

---

## 📊 Account Overview
| Metric | Value | Status |
| :--- | :--- | :--- |
| **Followers** | 0 | Launch Phase |
| **Total Tweets** | 0 | Initialized |
| **Total Impressions** | 0 | Tracking Active |

---

## 📝 Tweet & Thread Log

| Date | Tweet ID | Topic / Milestone | Impressions | Retweets | Likes | Replies | Link |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| 2026-08-14 | `pending` | **Anahata V2 Launch: The First Java ASI Container** | 0 | 0 | 0 | 0 | [View](https://x.com/AnahataASI) |

---

## 🛠️ One-Shot JIT Tweet Publisher & Stats Refresher Guide
Run this script via `NbJava.compileAndExecute` to post tweets or poll account metrics:

```java
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import uno.anahata.asi.swing.agi.tool.SwingAgiTool;

public class Anahata extends SwingAgiTool {
    @Override
    public Object call() throws Exception {
        Path keyPath = Path.of(System.getProperty("user.home"), ".anahata/twitter_api_keys.json");
        if (!Files.exists(keyPath)) {
            throw new IllegalStateException("Twitter API keys not found at: " + keyPath);
        }
        
        ObjectMapper mapper = new ObjectMapper();
        JsonNode keys = mapper.readTree(Files.readString(keyPath));
        String bearerToken = keys.get("bearerToken").asText();
        
        log("1. Authenticating with Twitter API v2...");
        // Example: Query account info or post a tweet
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("https://api.twitter.com/2/users/by/username/AnahataASI?user.fields=public_metrics"))
                .header("Authorization", "Bearer " + bearerToken)
                .GET()
                .build();
                
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        log("Response: " + response.body());
        return response.body();
    }
}
```
