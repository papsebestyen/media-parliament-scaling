import datetime as dt
import sys
import warnings

import matplotlib.patches as mpatches
import matplotlib.pyplot as plt
import seaborn as sns

warnings.filterwarnings("ignore")
sys.path.append("code/plots")

from plot_helper_functions import *

df = execute_formating().loc[
    lambda x: x["site"].isin(["index.hu", "origo.hu", "888.hu"])
]
figname = "slant_estimates_origo_case.png"


plt.figure(figsize=(10, 7))
sns.set_theme(style="whitegrid")
colors = ["#e01164", "#f4941c", "#011593"]
sns.set_palette(sns.color_palette(colors))

sns.lineplot(x="date", y="slant", hue="site", style="variable", data=df)

nyolcas = mpatches.Patch(color=colors[0], label="888.hu")
index = mpatches.Patch(color=colors[1], label="index.hu")
origo = mpatches.Patch(color=colors[2], label="origo.hu")

plt.legend(
    handles=[nyolcas, index, origo],
    loc=0,
    borderaxespad=1.0,
    frameon=False,
    title=False,
    numpoints=3,
    labels=["888.hu", "index.hu", "origo.hu"],
)
plt.title("Online hírportálok torzítottsága:\n az origo.hu esete", size=20, y=1.03)
plt.ylabel("Becsült torzítottság")
plt.xlabel(None)
plt.ylim(0.4, 0.65)
plt.xlim(dt.datetime(2010, 1, 1), dt.datetime(2022, 1, 1))

# change of editor
plt.axvline(dt.datetime(2014, 6, 2), color="#000000")
plt.annotate(
    "origo.hu\nszerkesztőváltás",
    xy=(3, 1),
    xycoords="axes fraction",
    xytext=(0.3, 0.15),
    textcoords="axes fraction",
    ha="center",
    va="center",
)

# change of owner
plt.axvline(dt.datetime(2015, 12, 7), color="#000000")
plt.annotate(
    "origo.hu\ntulajdonosváltás",
    xy=(3, 1),
    xycoords="axes fraction",
    xytext=(0.61, 0.15),
    textcoords="axes fraction",
    ha="center",
    va="center",
)

plt.savefig("figures/" + figname, dpi=1000)