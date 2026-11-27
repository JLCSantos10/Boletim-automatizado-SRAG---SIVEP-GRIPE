#!/usr/bin/env bash
# =============================================================================
# Painel SRAG — Linux e macOS
#
#   chmod +x abrir_painel_linux.sh     (só na primeira vez)
#   ./abrir_painel_linux.sh            (opcional: ./abrir_painel_linux.sh 5050)
# =============================================================================
set -euo pipefail

cd "$(dirname "$0")"
PORTA="${1:-4040}"

echo "====================================="
echo " Painel SRAG"
echo "====================================="
echo

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERRO: Rscript nao encontrado."
  echo "Instale o R:"
  echo "  Debian/Ubuntu : sudo apt install r-base"
  echo "  Fedora        : sudo dnf install R"
  echo "  macOS         : brew install r"
  exit 1
fi

echo "Rscript: $(command -v Rscript)"
echo "R: $(Rscript -e 'cat(R.version.string)')"
echo
echo "Abrindo o painel em http://127.0.0.1:${PORTA}"
echo "Na primeira execucao os pacotes podem levar alguns minutos para instalar."
echo
echo "Pressione Ctrl+C para encerrar."
echo

Rscript -e "shiny::runApp(getwd(), port = ${PORTA}, launch.browser = TRUE, host = '127.0.0.1')"
