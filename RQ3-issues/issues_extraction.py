import requests
import pandas as pd
import time

def extract_issue_info(owner, repo):
    url = f"https://api.github.com/repos/{owner}/{repo}/issues"
    params = {"state": "all", "per_page": 100}  # Include both open and closed issues, 100 per page
    headers = {"Accept": "application/vnd.github.v3+json",
        "Authorization": f"Token {'github_pat'}"}
    retries = 5  # Number of retries
    wait_time = 5  # Wait time in seconds
    issue_data = []

    while url:
        for _ in range(True):
            response = requests.get(url, params=params, headers=headers)

            if response.status_code == 200:
                issues = response.json()
                for issue in issues:
                    if "pull_request" not in issue:  # Filter out pull requests
                        issue_url = issue["url"]
                        state = issue["state"]
                        created_at = issue["created_at"]
                        closed_at = issue.get("closed_at")  # Handle closed issues without closed_at key
                        tags = [label["name"] for label in issue["labels"]]
                        issue_data.append({
                            "Issue URL": issue_url,
                            "State": state,
                            "Created At": created_at,
                            "Closed At": closed_at,
                            "Tags": tags
                        })

                # Check for pagination
                if "next" in response.links:
                    url = response.links["next"]["url"]
                else:
                    url = None  # No more pages
                break  # Exit retry loop if successful
            elif response.status_code == 403:  # Rate limit exceeded
                print("Rate limit exceeded. Waiting for retry...")
                time.sleep(wait_time)  # Wait before retrying
            else:
                print("Error:", response.status_code)
                url = None  # Exit pagination loop if error occurs
                break  # Exit retry loop if other error occurs

    return issue_data

# Read CSV file
df = pd.read_csv("Cdx.csv")

# Initialize list to store all issue data
all_issue_data = []

# Iterate over each repository link and extract issues
for link in df["RepoLink"]:
    if link.startswith("https://github.com/"):
        parts = link.split("/")
        owner = parts[-2]
        repo = parts[-1]
        print(f"Extracting issues from: {owner}/{repo}")
        issue_data = extract_issue_info(owner, repo)
        all_issue_data.extend(issue_data)
    else:
        print(f"Invalid GitHub repository link: {link}")

# Convert list of dictionaries to DataFrame
output_df = pd.DataFrame(all_issue_data)

# Save DataFrame to CSV file
output_df.to_csv("CdxIssues.csv", index=False)
