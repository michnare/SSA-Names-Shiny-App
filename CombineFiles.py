import pandas as pd
import glob
import re

# Step 1: Read and combine all text files
files = sorted(glob.glob(r"C:\Users\rmich\OneDrive\Desktop\Class Work\Exploratory Data Analysis\InfographicWeek2\names\*.txt"))
  # e.g., yob1880.txt, yob1881.txt, etc.
print(files)

all_data = []

for file in files:
    # Extract the year
    year = int(re.search(r'(\d{4})', file).group(1))
    df = pd.read_csv(file, names=["Name", "Gender", "Count"])
    df["Year"] = year
    all_data.append(df)

combined_df = pd.concat(all_data, ignore_index=True)

# Step 2: Pivot to make years columns
pivot_df = combined_df.pivot_table(
    index=["Name", "Gender"],   # rows
    columns="Year",             # columns
    values="Count",             # data to fill
    fill_value=0,               # fill missing combinations with 0
)

# Total births per year
year_totals = combined_df.groupby("Year")["Count"].sum()

# Add a relative popularity (%) column for each year
for year in year_totals.index:
    pivot_df[year] = pivot_df[year] / year_totals[year] * 100

# Step 3: Reset the index for a clean table
pivot_df.reset_index(inplace=True)

# Step 4: Optionally save to file
pivot_df.to_csv("babynames_pivot.csv", index=False)
# or:
# pivot_df.to_excel("babynames_pivot.xlsx", index=False)

print("✅ Pivot table saved as 'babynames_pivot.csv'")


