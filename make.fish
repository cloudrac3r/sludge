#!/usr/bin/env fish

# /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
# | Copyright 2024 Cadence Ember    |
# | See `sludge-game/LICENSE.txt`   |
# | for copying & reuse conditions. |
# \_________________________________/

if test -z "$argv"
    set argv unix windows
end

if test "$TOOLBOX_NAME" = wine
    set argv windows
end

function do
    echo $argv
    $argv
    or exit
end

if grep -q -E 'debug-mode.*t' sludge-game/main.rkt
    echo "HEY! Debug mode is on. Turn this off for the final build."
end

if not grep -q -E 'enable-designs.*t' sludge-game/main.rkt
    echo "You must enable designs for release builds. Set `enable-designs #t'."
    exit 1
end

if contains unix $argv
    echo 'Making unix...'
    ~/.racket-bc/bin/raco pkg install --auto req-lib
    do ~/.racket-bc/bin/raco req -d
    do ~/.racket-bc/bin/raco exe --gui -o sludge sludge-game/main.rkt
    do ~/.racket-bc/bin/raco distribute sludge-dist sludge
    do rm -f sludge-dist.tar.gz
    do tar -caf sludge-dist.tar.gz sludge-dist
end

if contains windows $argv
    if not command -q wine; and not contains no-retry $argv
        toolbox run -c wine fish (status current-filename) windows no-retry
    else
        echo 'Making windows...'
        wine '/var/home/cadence/.wine/dosdevices/c:/Program Files/Racket/raco.exe' pkg install --auto req-lib
        do wine '/var/home/cadence/.wine/dosdevices/c:/Program Files/Racket/raco.exe' req -d
        do wine '/var/home/cadence/.wine/dosdevices/c:/Program Files/Racket/raco.exe' exe --gui -o sludge.exe sludge-game/main.rkt
        do wine '/var/home/cadence/.wine/dosdevices/c:/Program Files/Racket/raco.exe' distribute sludge-dist-windows sludge.exe
        do rm -f sludge-dist-windows.zip
        do 7za a sludge-dist-windows.zip sludge-dist-windows
    end
end
