import pandas as pd
from datetime import datetime

# Read the CSV file
df = pd.read_csv("SpdxIssues.csv")

# Function to calculate time difference
def calculate_time_difference(row):
    if row['State'] == 'closed':
        date_str1 = row['Created At'].rstrip('Z')
        date_str2 = row['Closed At'].rstrip('Z')
        date1 = datetime.strptime(date_str1, "%Y-%m-%dT%H:%M:%S")
        date2 = datetime.strptime(date_str2, "%Y-%m-%dT%H:%M:%S")
        time_diff = date2 - date1
        return time_diff.total_seconds()
    # -1 indicates an open issue which means its resolve time can't be calculated
    return -1

# Apply the function to calculate time difference and create a new column 'time_diff'
df['time_diff'] = df.apply(calculate_time_difference, axis=1)

# Write the updated DataFrame back to CSV
df.to_csv("SpdxIssues_with_time_diff.csv", index=False)
