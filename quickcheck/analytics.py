import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

with open('analytics_logs_2', 'r') as f:
    s = f.read().split('\n')[1:-1]

parseData = lambda s: list(map(int, s.split('%')[1][2:-1].split(',')))

data = list(map(parseData, s))

heights = []
sizes = []
leaves = []

for d in data:
    h, s, l = d
    heights.append(h)
    sizes.append(s)
    leaves.append(l)

df = pd.DataFrame({'height': heights, 'nodes':  sizes, 'leaves':leaves})


plt.figure(figsize=(10,8))
plt.grid(True)
sns.histplot(df, x="height", bins=len(set(heights)))
plt.xlabel('Height')
plt.show()

plt.figure(figsize=(10,8))
plt.grid(True)
sns.histplot(df, x="nodes", bins=len(set(sizes)))
plt.xlabel('Number of nodes')
plt.show()

plt.figure(figsize=(10,8))
plt.grid(True)
sns.histplot(df, x="leaves", bins=len(set(leaves)))
plt.xlabel('Number of leaves')
plt.show()

df_less_data = df.loc[(df['nodes'] >= 32) & (df['nodes'] <= 38)]

plt.figure(figsize=(10,8))
plt.grid(True)
sns.violinplot(data=df_less_data, x="nodes", y="height")
plt.xlabel('Number of nodes')
plt.ylabel('Tree Height')
plt.show()

plt.figure(figsize=(10,8))
plt.grid(True)
sns.violinplot(data=df_less_data, x="nodes", y="leaves")
plt.xlabel('Number of nodes')
plt.ylabel('Number of leaves')
plt.show()
