# Final Project for Social Networks

## Research Question
**Can Amazon optimize its sales by leveraging product co-purchasing patterns?**

## Data Collection

### Initial Crawling
- **Date:** June 1, 2003
- **Method:** A web crawl of Amazon.com based on the “Customers Who Bought This Item Also Bought” feature (Leskovec, Adamic, Huberman).

### Dataset Details
- **Nodes:** 403,394
- **Edges:** 3,387,388
- **Representation:** Directed edges indicate frequent co-purchases between products.

### Limitations
- Lacked product names and IDs, limiting insights into specific product links.

### Subset Selection
- **Cluster:** Highly interconnected cluster of 355 nodes and 812 edges was selected.
- **Components:** 7 components identified; analysis focused on the largest.

## Descriptive Analysis

### K Core Decomposition
- **Max k-core:** 6
- **Key Nodes:** 16770, 16771, 35897, 35898
  - These nodes are highly influential and should be heavily advertised.

### Community Structure
- **Algorithm:** Walktrap
- **Clusters Identified:** 32
- **Modularity Score:** 0.8198
  - Indicates strong community structures; Amazon should group and market products within a cluster together.

### Indegree and Outdegree
- **Indegree:** Focus recommendations on key connections.
- **Outdegree:** Common trio in buying patterns.
  - **Distribution Plot (Figure 6):** Peak at 2, suggesting items are commonly bought in groups of 3.

## Hypothesis Testing
1. **Network is sparse; low probability of random ties** (Supported).
2. **Mutual purchases are likely** (Supported).
3. **Popular products don’t always lead to many co-purchases** (Contradicted).
4. **Products frequently bought together attract more purchases** (Supported, but inconclusive).

## ERGM Model Insights
- **Network’s Sparsity:** Confirmed.
- **Mutual Purchase Likelihood:** Confirmed.
- **Tie Probability:** 0.5%
- **Goodness of Fit:** 
  - Good fit with outdegree patterns.
  - Less so with indegree.

### Goodness of Fit
- **Convergence:** Model converged, indicating usability.
- **Trace Plots:** Stable around 0, no nonlinear patterns, indicating usability.
- **Sample Statistic Deviations:** Normally distributed, unimodal with a mean of 0, indicating usability.
- **P-values for Outdegree:** > 0.05, indicating a good fit.
- **P-values for Indegree:** < 0.05, indicating a poor fit.

## Recommendations for Amazon
1. **Leverage Product Clusters:** Display clusters on the homepage to enhance customer discovery and sales.
2. **Bundle Strategically:** Use outdegree insights to create enticing bundled offers, especially during events like Prime Day.
3. **Promote High Centrality Products:** Highlight items with high centrality scores to drive sales.
4. **Enhance Recommendations:** Suggest items with high eigenvector centrality, indegrees, betweenness centrality, and hub scores.
5. **Investigate Further:** Obtain product names to refine recommendations and understand why certain items are central in purchasing patterns.

By implementing these strategies, Amazon can significantly boost its sales and customer satisfaction.


