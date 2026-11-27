@echo off
setlocal enabledelayedexpansion
cd /d "%~dp0"

echo =====================================
echo  Painel SRAG
echo =====================================
echo.

set RSCRIPT_PATH=

echo Procurando Rscript no PATH...
where Rscript >nul 2>nul
if %errorlevel% equ 0 (
    for /f "delims=" %%i in ('where Rscript') do (
        if not defined RSCRIPT_PATH set "RSCRIPT_PATH=%%i"
    )
)

if not defined RSCRIPT_PATH (
    echo Procurando em Program Files...
    for /d %%D in ("C:\Program Files\R\R-*") do (
        if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT_PATH=%%D\bin\x64\Rscript.exe"
        if exist "%%D\bin\Rscript.exe"     set "RSCRIPT_PATH=%%D\bin\Rscript.exe"
    )
)

if not defined RSCRIPT_PATH (
    echo Procurando em AppData...
    for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do (
        if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT_PATH=%%D\bin\x64\Rscript.exe"
        if exist "%%D\bin\Rscript.exe"     set "RSCRIPT_PATH=%%D\bin\Rscript.exe"
    )
)

if not defined RSCRIPT_PATH (
    echo.
    echo ERRO: R nao encontrado no sistema.
    echo Instale o R em https://cran.r-project.org/bin/windows/base/
    echo.
    pause
    exit /b 1
)

echo.
echo Rscript encontrado em:
echo !RSCRIPT_PATH!
echo.
echo Abrindo o painel no navegador (http://127.0.0.1:4040)...
echo.
echo Na primeira execucao os pacotes podem levar alguns minutos para instalar.
echo Na primeira vez que voce escolher uma UF, o historico completo e baixado
echo da API (pode levar alguns minutos). Depois disso o painel abre na hora.
echo.
echo FECHE ESTA JANELA PARA ENCERRAR O PAINEL.
echo.

"!RSCRIPT_PATH!" -e "shiny::runApp(getwd(), port = 4040, launch.browser = TRUE)"

echo.
echo Painel encerrado.
pause
endlocal & exit /b 0
