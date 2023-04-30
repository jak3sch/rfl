#!/bin/bash

# Remote-Repository URL
REPO_URL="https://$GITLAB_USER:$GITLAB_PASSWORD@gitlab.com/jakob-eschler/data-science/fantasy-football/rfl.git"

# CSV-Datei herunterladen
curl --user "$GITLAB_USER:$GITLAB_PASSWORD" \
     "https://gitlab.com/api/v4/projects/$CI_PROJECT_ID/jobs/artifacts/$CI_COMMIT_REF_NAME/download?job=$CI_JOB_NAME" \
     --output data/war/output.csv

# Aktuellen Stand des Remote-Repositories herunterladen
git pull $REPO_URL main

# Hinzufügen der CSV-Datei zum Git-Index
git add data/war/output.csv

# Committen der Änderungen
git commit -m "Automatisches Hinzufügen der CSV-Datei"

# Pushen der Änderungen zum Remote-Repository
git push $REPO_URL HEAD:main
