#!/bin/bash

# GitLab Personal Access Token
TOKEN="glpat-EERdyyy6WGw1_JxDGef8"

# Remote-Repository URL
REPO_URL="https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl.git"

# CSV-Datei herunterladen
curl --header "PRIVATE-TOKEN: $CI_JOB_TOKEN" \
     "https://gitlab.com/api/v4/projects/$CI_PROJECT_ID/jobs/artifacts/$CI_COMMIT_REF_NAME/download?job=$CI_JOB_NAME" \
     --output output.csv

# Hinzufügen der CSV-Datei zum Git-Index
git add output.csv

# Committen der Änderungen
git commit -m "Automatisches Hinzufügen der CSV-Datei"

# Pushen der Änderungen zum Remote-Repository
git push $REPO_URL HEAD:$CI_COMMIT_REF_NAME
