# Codebook

Mapping of dataset variables (`data/Denmark_Dataset.xlsx`) to survey items. The full instrument and official codebook are in the manuscript's Appendix A (Survey) and Appendix B (Codebook). All attitudinal items (`Q18`–`Q32`) use a five-point Likert scale: 1 = strongly disagree, 2 = somewhat disagree, 3 = undecided, 4 = somewhat agree, 5 = strongly agree, with a "not enough information" option where appropriate. Reverse-scored items are handled in the analysis script.

## Classifier

| Variable | Meaning |
| --- | --- |
| `USAGE` | AI-use classifier: `Use`, `I don't use`, `I don't know` |

## Section 1 — Sociodemographic characteristics

| Variable | Item |
| --- | --- |
| `Q1` | Country of organization |
| `Q2` | Organization (free text; standardized in analysis to `Organization_clean`) |
| `Q3` / `Q3_4_text` | Level of government (+ "other" free text) |
| `Q4` / `Q4_11_text` | Domain (+ "other" free text) |
| `Q5` / `Q5_3_text` | Role (+ "other" free text) |
| `Q6` / `Q6_3_text` | Work area (+ "other" free text) |
| `Q7` | Number of employees within the organization |
| `Q8` | Sex |
| `Q9` | Age |
| `Q10` | Years of work experience in the public sector |
| `Q11` | Years of total work experience |
| `Q12` | Education |
| `Q13` | Field of education |

## Section 2 — AI exposure and use

| Variable | Item |
| --- | --- |
| `Q14a`–`Q14m` (+ `Q14k_text`) | AI tool types used (binary indicators; multiple selection) |
| `Q15` | Frequency of AI use |
| `Q16` | Self-rated experience with AI tools |
| `Q17` | (AI exposure item — see Appendix A) |

## Likert construct sections (AI users)

Items are grouped into the questionnaire's predefined constructs and aggregated into composite scores in the analysis. Section headings below follow the manuscript.

| Item block | Construct / topic |
| --- | --- |
| `Q18a`–` … ` | Perceived effects of AI on work performance |
| `Q19`–`Q22` | Learning and interaction with AI tools |
| `Q23a`–`Q23k` | Social and organizational support |
| `Q24a`–`Q24g` | Organizational readiness |
| `Q25a`–`Q25f` | Proactive individual AI use and exploration |
| `Q26a`–`Q26e` | Adaptability to AI tools and changing work practices |
| `Q27a`–`Q27e` | Resilience under AI-related change |
| `Q28a`–`Q28l` | Employment impact, governance, employee voice, managerial support, workplace experience |
| `Q29a`–`Q29i` | Efficiency, workload reduction, quality, accuracy |
| `Q30a`–`Q30h` | Citizen/stakeholder engagement and AI governance |
| `Q31a`–`Q31i` | Transparency, disclosure, and legal compliance |

### Reasons for non-use (non-users)

| Item block | Topic |
| --- | --- |
| `Q32a`–`Q32l` | Barriers to AI adoption (skills, trust, privacy, organizational, professional) |

### Open-ended

| Variable | Item |
| --- | --- |
| `Q33` | "Can you share some general views/words about how you see AI?" (free text) |

> The exact wording of every `Q18`–`Q31` item is listed in the manuscript's Appendix B. This codebook summarizes the block-to-construct mapping used by the analysis; consult Appendix B for item-level text.
