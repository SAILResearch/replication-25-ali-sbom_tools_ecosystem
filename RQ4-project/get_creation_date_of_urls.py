import csv
import requests

def get_repo_creation_date(repo_url, access_token):
    headers = {'Authorization': f'token {access_token}'}
    response = requests.get(repo_url, headers=headers)
    if response.status_code == 200:
        repo_info = response.json()
        creation_date = repo_info['created_at']
        return creation_date
    else:
        print(f"Failed to get creation date for {repo_url}. Status code: {response.status_code}")
        return None

def main():
    access_token = ''
    input_file = 'top250_spdx_cdx_rq4.csv'
    output_file = 'top250_spdx_cdx_rq4_with_dates.csv'

    with open(input_file, 'r', newline='') as infile, \
            open(output_file, 'w', newline='') as outfile:
        reader = csv.DictReader(infile)
        fieldnames = reader.fieldnames + ['creation_date']
        writer = csv.DictWriter(outfile, fieldnames=fieldnames)
        writer.writeheader()

        for row in reader:
            repo_url = row['link']
            # Assuming the repo URL format is like "https://github.com/owner/repo"
            if 'github.com' in repo_url:
                repo_owner, repo_name = repo_url.split('/')[-2:]
                repo_api_url = f"https://api.github.com/repos/{repo_owner}/{repo_name}"
                creation_date = get_repo_creation_date(repo_api_url, access_token)
                row['creation_date'] = creation_date if creation_date else 'N/A'
                writer.writerow(row)
                print(f"Creation date of {repo_owner}/{repo_name}: {creation_date}")
            else:
                row['creation_date'] = 'N/A'
                writer.writerow(row)
                print(f"Invalid GitHub URL: {repo_url}")

if __name__ == "__main__":
    main()
