#' Configuration and information about question text and choices
library(stringr)
library(dplyr)

make_options_df <- function(options_vec, q_num) {
    options_df <- data.frame(
        question = paste0("Q", q_num, "_", 1:length(options_vec)),
        label = options_vec) 
}

Q3_Text <- "Ich nutze KI-Anwendungen aktiv für unsere Vereinsarbeit."

Q4_Text <- "In den folgenden Bereichen nutzen ich KI-Anwendungen für meine Vereinsarbeit aktiv:"

Q4_Options <- c(
    "Kommunikation (z.B. E-Mail-Management, Chatbots)",
    "Verwaltung (z.B. Mitgliederverwaltung, Buchhaltung)",
    "Öffentlichkeitsarbeit (z.B. Social Media, Content-Erstellung allgemein)",
    "Veranstaltungsmanagement", 
    "Fundraising",
    "Datenanalyse und Berichterstattung",
    "Sonstiges und zwar:"
)  

Q4_Options_map <- Q4_Options %>% make_options_df(4)

Q5_Text <- "Folgende Herausforderungen sehe ich bei der aktiven Nutzung von KI-Anwendungen für die Vereinsarbeit? (Pflichtfrage)"
Q5_Text_cleaned <- "Folgende Herausforderungen sehe ich bei der aktiven Nutzung von KI-Anwendungen für die Vereinsarbeit:"
Q5_Options <- c(
    "Datenschutzbedenken", "Hohe Kosten", "Mangelndes Know-how",
    "Technische Schwierigkeiten bei der Umsetzung",
    "Skepsis von Mitgliedern und Mitarbeitenden", "Ethische Bedenken",
    "Kosten-Nutzen-Relation zu schlecht",
    "Schwierigkeiten, die richtigen KI-Anwendungen zu finden",
    "Sonstiges und zwar:"
) 
Q5_Options_map <- Q5_Options %>% make_options_df(5)

Q5_Options_cleaned <- c(
    "Datenschutz", "Hohe Kosten", "Know-how",
    "Techn. Schwierigkeiten",
    "Skepsis", "Ethische Bedenken",
    "Kosten-Nutzen-Relation",
    "Auswahlschwierigkeiten",
    "Sonstiges"
) 

Q5_Options_cleaned_map <- Q5_Options %>% make_options_df(5)

Q6_Text <- "Ich nutze KI für meine Vereinsarbeit ..."

Q7_Text <- "Der Nutzen von KI-Anwendungen für die Vereinsarbeit ist..."

Q8_Text <- "Meiner Meinung nach ermöglicht die Nutzung von KI in der Vereinsarbeit..."
Q8_Options <- c("Effizienzsteigerung", "Zeitersparnis",
    "Bessere Kommunikation und Erreichbarkeit für Mitglieder",
    "Mehr Transparenz in der Vereinsarbeit",
    "Erleichterung der Entscheidungsfindung durch Datenanalyse",
    "Professionalisierung der Vereinsarbeit", "Sonstiges und zwar"
)

Q8_Options_map <-  Q8_Options %>% make_options_df(8)
Q8_Options_cleaned <- c("Effizienzsteigerung", "Zeitersparnis",
    "Bessere Kommunikation u. Erreichbarkeit für Mitglieder",
    "Mehr Transparenz in der Vereinsarbeit",
    "Erleichterung Entscheidungsfindung durch Datenanalyse",
    "Professionalisierung der Vereinsarbeit", "Sonstiges und zwar"
)
Q8_Options_cleaned_map <-  Q8_Options_cleaned %>% make_options_df(8)


Q9_Text <- "Deine Gesamteinschätzung zu KI-Anwendungen im Verein: Hype oder Mehrwert für das Ehrenamt?"

Q10_Text <- "Folgende KI-Anwendungen nutze ich aktiv für meine Vereinsarbeit?"
Q10_Options <- c(
    "ChatGPT (kostenlose Version)", "ChatGPT (kostenpflichtige Version)", "Microsoft Copilot", "Perplexity", "Claude", "Hugging Face", "Gemini (Google)", "KI-Bildgeneratoren (z.B.: Midjourney, Canva)",
    "Vereinssoftware mit KI-Funktionen (z.B.: Clubdesk, Campai, Lexoffice)", "KI für Social Media Management (z.B.: Hootsuite, Buffer)", "KI basierte Übersetzungstools (z.B.: DeepL)", "Sprachassistenten (z.B.: Alexa)", "Sonstige und zwar:"
)
Q10_Options_map <- Q10_Options %>% make_options_df(10)


Q11_Text <- "Welche KI-Anwendung empfiehlst du dringend anderen Vereinen und wieso?"

Q12_Text <- "Folgende Werte sind für mich bei der Nutzung von KI-Anwendungen besonders wichtig (Pflichtfrage)"
Q12_Options <- c(
    "Transparenz (Nachvollziehbarkeit der Entscheidungen und Prozesse der KI)", "Sicherheit (Schutz der persönlichen Daten von Mitgliedern)", "Fairness (keine Diskriminierung oder Benachteiligung durch KI)",
    "Vertrauen (KI sollte zuverlässig und verständlich sein)", "Autonomie (KI sollte unterstützen, aber nicht die Kontrolle übernehmen)", "Soziales Miteinander (Berücksichtigung der Auswirkungen auf die Gemeinschaft)", "Sonstiges und zwar:"
) 
Q12_Options_map = Q12_Options %>% make_options_df(12)


Q13_Text <- "Wie wirkt sich KI auf den sozialen Zusammenhalt im Verein aus?"

Q14_Text <- "Wie wirkt sich KI auf die Werte deines Vereins aus?"

Q15_Text <- "Offene Frage: Wenn du einem Entwickler von KI etwas mit auf den Weg geben könntest, was wäre es? Was denkst du, sollte man bei der Entwicklung von KI-Anwendungen besonders berücksichtigen?"

Q16_Text <- "Worin soll dich KI unterstützen?"
Q16_Options <- c("Automatisierung von Verwaltungsarbeit", "Datenanalyse und Auswertung (z.B. Trends)", "Mitgliedergewinnung durch Marketing", "Beantworten von Fragen durch Chatbots", "Planung und Organisation von Veranstaltungen (z.B. Einladungen)", "Feedback-Analyse (z.B. Mitgliederzufriedenheit)", "Schulung von Mitgliedern (z.B. E-Learning)", "Sonstiges") 
Q16_Options_map <- Q16_Options %>% make_options_df(16)

Q17_Text <- "Wie hoch ist der Bedarf an KI in deiner Vereinsarbeit?"

Q18_Text <- "Was brauchst du, um KI in deiner Vereinsarbeit aktiv zu nutzen? (Pflichtfrage)"

Q18_Options <- c("Schulungen & Workshops", "Coaching & Begleitung", "Finanziell Förderung", "Technische Unterstützung", "Ethische und rechtliche Sicherheit (&Beratung)", "Zugang zu KI-Tools", "Offenheit & Interesse", "Sonstiges:") 
