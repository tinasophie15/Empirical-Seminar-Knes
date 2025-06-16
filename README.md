# Emotional Attachment in Educational Games: Does emotional attachment to a game's character lead to better learning outcomes?

This repository contains the R scripts, data processing steps and analysis for a study conducted in the context of the subject Empirical Seminar. The study investigates whether emotional attachment to a game character (through narrative elements) enhances learning outcomes in a digital game-based learning environment.

## Project Summary

**Research Question:**  
Does emotional attachment to a game's character lead to better learning outcomes?

**Game Scenario:**  
Participants interacted with an educational memory game featuring an archaeologist dog. In the narrative version, the dog is called Montana and Montana’s backstory aimed to create emotional engagement. In the non-narrative version, the same game mechanics were presented without story or character framing.

## Experimental Design

- **Design:** Between-subjects experiment with two conditions:
  - `B1`: Non-Narrative Group (no story)
  - `B2`: Narrative Group (Montana’s backstory)
- **Data Sources:**
  - Game performance metrics (e.g., accuracy, rounds, time)
  - Pre/post questionnaires (e.g., Pet Attachment Questionnaire)
- **Sample:** 29 participants (pilot phase), in total: 55 participants completed the study

## Analysis Methods

- **Preprocessing:**
  - Data cleaning and exclusion of test participants
  - Conversion of Likert-scale responses to numeric values
- **Statistical Tests:**
  - Independent t-tests & ANOVA
  - Mann-Whitney U tests
  - Pearson & Spearman correlations (overall and by group)
- **Visualizations:**
  - Boxplots, scatterplots with regression lines, violin plots
  - Correlation matrix and line charts for learning progression

## Key Findings

- Both groups improved over time, but no significant difference in learning accuracy was found between B1 and B2.
- Time-on-task was lower in the narrative group.
- Emotional attachment showed no significant correlation with performance overall.
- However, **group-specific trends revealed opposing directions** in correlation:
  - B1 showed a slight positive correlation.
  - B2 showed a slight negative correlation.

These opposing trends might explain why the overall correlation appeared insignificant.

## References

- Alexiou et al. (2022). Narrative and Aesthetics in Serious Games. [DOI](https://doi.org/10.1108/ITP-08-2019-0435)
- Bai et al. (2020). Gamification & Learning Outcomes. [DOI](https://doi.org/10.1016/j.edurev.2020.100322)
- Zilcha-Mano et al. (2011). Attachment to Pets. [DOI](https://doi.org/10.1016/j.jrp.2011.04.001)
- Mayer (2020). Cognitive Foundations of Game-Based Learning.
- Schaffer (2023). Task Significance in Digital Games.

## Contact

**Christina Knes**  
Email: christina.knes@student.tugraz.at  
University of Graz / Graz University of Technology  
