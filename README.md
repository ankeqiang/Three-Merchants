# Three Merchants

## Zhu Baosan, Yu Qiaqing, and Wang Xiaolai in Modern Shanghai

This repository contains research data, R scripts, network-analysis outputs, Cytoscape workspaces, and figures for a comparative study of three prominent Shanghai merchants:

- **Zhu Baosan 朱葆三** (ZBS)
- **Yu Qiaqing 虞洽卿** (YQQ)
- **Wang Xiaolai 王晓籁 / 王曉籟** (WXL)

The project examines the changing economic, institutional, social, and public networks of these merchants across the late Qing and Republican periods. It combines biographical and chronological reconstruction with computational analysis of press corpora, person-organization affiliation networks, network centrality, principal component analysis, and periodized visualization.

The repository is a **research archive**, not an R package. Its directories preserve both analysis-ready outputs and parts of the working computational pipeline used to produce them.

---

## Research focus

The project asks how major Shanghai merchants built, maintained, and transformed their influence across business, politics, civic institutions, philanthropy, and public life. Rather than treating elite status as a fixed attribute, the analysis follows the changing composition and structure of each merchant's networks over time.

The comparative design makes it possible to examine:

- the relationship between business activity and public influence;
- changing person-organization affiliation networks;
- shifts in network centrality across historical periods;
- the relationship between economic, political, civic, and philanthropic roles;
- differences between Chinese- and English-language press visibility;
- the evolution of each merchant's public profile;
- similarities and differences in the trajectories of Zhu Baosan, Yu Qiaqing, and Wang Xiaolai.

---

## Abbreviations and file-name conventions

| Abbreviation | Merchant / meaning |
|---|---|
| `ZBS` / `zbs` | Zhu Baosan 朱葆三 |
| `YQQ` / `yqq` | Yu Qiaqing 虞洽卿 |
| `WXL` / `wxl` | Wang Xiaolai 王晓籁 / 王曉籟 |
| `P1`, `P2`, etc. | Historical sub-periods used in the network analysis |
| `mc` | Main (largest connected) network component |
| `Deg` | Degree-based filtering or visualization threshold |
| `centralities` | Node-level network centrality results |
| `sb` | *Shenbao* material |
| `tm` / `TM` | Topic-modeling workflow/output |
| `Cy` | Cytoscape-oriented node/edge export |

Some older Cytoscape files use the prefix `yyq` for Yu Qiaqing. This is a legacy file-name variant of `YQQ` and refers to the same person.

---

## Repository structure

```text
Three-Merchants/
|
|-- Cytoscape_All_Files/
|-- WXL_Figures/
|-- WXL_Used4analysis/
|-- WXL_centralities/
|-- YQQ_Figures/
|-- YQQ_centralities/
|-- ZBS_Data/
|-- ZBS_Figures/
|-- ZBS_Scripts/
|-- ZBS_centralities_data/
|-- .gitattributes
`-- README.md
```

The repository currently contains a particularly complete scripted workflow for **Zhu Baosan**, while the Yu Qiaqing and Wang Xiaolai directories primarily preserve analytical outputs, centrality tables, figures, and Cytoscape sessions.

---

## `Cytoscape_All_Files/`

This directory contains saved **Cytoscape session files (`.cys`)** for periodized network analysis.

The sessions are organized by merchant and sub-period:

```text
zbsP1.cys ... zbsP5.cys

yyq_P1.cys ... yyq_P6.cys

wxl_P1.cys ... wxl_P5.cys
```

These files preserve interactive network layouts and are useful for inspecting the person-organization affiliation networks beyond the static PNG figures included elsewhere in the repository.

They should be opened with Cytoscape.

---

## Zhu Baosan (`ZBS`)

### `ZBS_Data/`

Contains working and derived datasets used in the Zhu Baosan analysis. These files support the reconstruction of press visibility, named entities, affiliation networks, and other intermediate analytical stages.

### `ZBS_Scripts/`

Contains the main R workflow for Zhu Baosan. The numbered scripts preserve the approximate order in which different components of the analysis were developed.

Important groups include:

- press-corpus retrieval and preparation;
- temporal analysis of *Shenbao* and English-language press material;
- entity-based person and organization extraction;
- construction of person-organization affiliation networks;
- extraction of largest connected components;
- calculation and export of network measures;
- period-specific topic modeling;
- timeline production;
- network-biographical analysis.

For example, `4-zbs_ntwks1.R` builds periodized affiliation networks from person and organization entities. It constructs two-mode person-organization networks, exports node and edge files for Cytoscape, extracts the largest connected component, and prepares the networks for centrality analysis and visualization.

The ZBS network workflow uses five main periods:

1. **1888-1899**
2. **1900-1912**
3. **1913-1919**
4. **1920-1926**
5. **1927-1949**

### `ZBS_centralities_data/`

Contains period-specific centrality tables for the principal network component:

```text
mc1centralities.csv
mc2centralities.csv
mc3centralities.csv
mc4centralities.csv
mc5centralities.csv
```

These files contain the node-level measures used to compare structural position and prominence across periods.

### `ZBS_Figures/`

Contains figures generated during the Zhu Baosan analysis, including:

- period-specific network visualizations;
- degree-filtered network graphs;
- press timelines;
- CNKI-related statistics;
- top-ranked entity/network figures;
- PCA visualizations in `zbs_PCA_Graphs/`.

The figure filenames often preserve the analytical settings used to create them. For example, `P3`, `mc`, and `Deg700` indicate period 3, the main component, and a degree-related filtering/visualization threshold.

---

## Yu Qiaqing (`YQQ`)

### `YQQ_centralities/`

Contains centrality results for six historical periods:

```text
mc1centralities.csv
mc2centralities.csv
mc3centralities.csv
mc4centralities.csv
mc5centralities.csv
mc6centralities.csv
```

The six-period structure corresponds to the periodized reconstruction of Yu Qiaqing's evolving network.

### `YQQ_Figures/`

Contains the principal visual outputs for the Yu Qiaqing analysis, including periodized network and chronological figures.

Interactive network versions are stored in `Cytoscape_All_Files/` under the legacy `yyq_` prefix.

---

## Wang Xiaolai (`WXL`)

### `WXL_centralities/`

Contains centrality results for five historical periods:

```text
mc1centralities.csv
mc2centralities.csv
mc3centralities.csv
mc4centralities.csv
mc5centralities.csv
```

### `WXL_Used4analysis/`

Contains network files and visualizations retained for the final analytical stages. Filenames encode period, component, and filtering choices, for example:

```text
wxl_P1Deg50.csv
wxl_P1Deg70.csv
wxl_P1Deg80.csv
wxl_P1mc.csv
```

The directory therefore preserves both analysis-ready network tables and the corresponding graphical outputs used to compare alternative thresholds and network representations.

### `WXL_Figures/`

Contains principal Wang Xiaolai figures, including:

- chronological press-visibility plots;
- CNKI statistics;
- top-ranked results;
- *Shenbao* timelines;
- combined Chinese- and English-language press timelines.

Interactive network sessions for the five WXL periods are stored in `Cytoscape_All_Files/`.

---

## Computational workflow

The broad workflow represented in the repository is:

```text
Historical press corpora / biographical data
                |
                v
      Search and document retrieval
                |
                v
       Cleaning and preparation
                |
                v
       Named-entity extraction
         (PERSON / ORG)
                |
                v
 Person-organization edge construction
                |
                v
      Periodized network graphs
                |
                v
 Largest connected component (`mc`)
                |
                v
       Centrality calculation
                |
                +-------------------+
                |                   |
                v                   v
        Cytoscape sessions      R visualizations
                |                   |
                +---------+---------+
                          |
                          v
             Comparative interpretation
```

The network workflow uses co-occurrence of named persons and organizations within documents to construct affiliation networks. These are then divided into historical sub-periods so that changes in network structure can be examined chronologically rather than only in aggregate.

---

## Press data and HistText

The R scripts use **HistText** to search and retrieve documents from historical text corpora. In the ZBS workflow, the scripts query the revised *Shenbao* corpus and use multiple name forms to improve retrieval.

The working pipeline also compares Chinese-language press evidence with English-language press material. Several filenames retain `prq` / `proq` abbreviations associated with that part of the workflow.

Because corpus access can depend on the local HistText installation and available collections, reproducing the document-retrieval stages may require access to the same corpora used when the scripts were created.

---

## R dependencies

The ZBS scripts use packages including:

```r
histtext
lubridate
ggplot2
tidygraph
igraph
tidyverse
tidytext
httr
jsonlite
```

Not every script requires every package.

The network workspaces additionally require **Cytoscape** for opening `.cys` files.

---

## Network measures and PCA

The repository preserves period-specific centrality tables for all three merchants. These outputs are used to compare the changing structural positions of people and organizations within each merchant's network.

The analysis also includes **principal component analysis (PCA)** of centrality measures. PCA graphics are preserved among the figure outputs, especially in the ZBS material. This provides a synthetic way to compare nodes across multiple centrality dimensions rather than relying on a single measure of prominence.

---

## Reproducibility notes

This repository reflects the successive stages of a historical research project. Several conventions are therefore worth keeping in mind:

1. **File naming is historical.** Capitalization and prefixes are not fully standardized across directories.
2. **`YQQ` and `yyq` refer to the same merchant.** The `yyq` form occurs primarily in older Cytoscape filenames.
3. **Intermediate files are retained.** Some CSV and image files represent experiments with alternative network thresholds rather than final publication figures.
4. **Saved Cytoscape sessions are analytical objects.** They preserve layout and interactive network state that cannot be reconstructed from PNG files alone.
5. **Corpus-dependent scripts may not run in a clean R installation without access to the corresponding HistText corpora.**
6. **The repository is not yet fully pipeline-driven.** The ZBS analysis is the most fully represented in scripts; some YQQ and WXL upstream processing was conducted outside the currently preserved root structure.

For historical verification, analytical outputs should always be checked against the underlying source records and corpus documents.

---

## Suggested entry points

For readers primarily interested in the **network analysis**, begin with:

```text
Cytoscape_All_Files/
ZBS_centralities_data/
YQQ_centralities/
WXL_centralities/
```

For readers interested in the **computational workflow**, begin with:

```text
ZBS_Scripts/1-zbssb.R
ZBS_Scripts/4-zbs_ntwks1.R
```

For readers interested in the **figures used in the comparative analysis**, begin with:

```text
ZBS_Figures/
YQQ_Figures/
WXL_Figures/
```

---

## Research context

The repository supports a comparative historical study of Shanghai merchant elites and the changing relationship among commerce, politics, social organization, philanthropy, and public visibility. By combining close historical research with structured data, text mining, network analysis, and visualization, the project seeks to reconstruct elite influence as a dynamic set of relationships rather than as a fixed social category.

The materials should be read as computational evidence in support of historical analysis, not as substitutes for the underlying primary sources.

---

## Author / project

**Christian Henriot**  
Aix-Marseille Université  
ENP-China

Repository: `ankeqiang/Three-Merchants`
