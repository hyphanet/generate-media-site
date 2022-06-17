Guile Media Site
================

A generator for simple streaming media sites, optimized for insertion into Freenet.

Adjust [site/template.html](site/template.html) to make it match your style.

See [site/gms.scm](site/gms.scm) for additional information.

## Usage

Run on debian (example):

    sudo apt install mercurial ffmpeg mplayer grep sed wget guile-3.0
    hg clone https://hg.sr.ht/~arnebab/guile-media-site
    (cd guile-media-site/media && wget https://cdn.media.ccc.de/events/divoc/bb3/webm-sd/divoc-bb3-48946-deu-eng-Briefgeheimnis_wahren_-_Chatkontrolle_stoppen_webm-sd.webm)
    (cd guile-media-site/site && chmod +x ./gms.scm && ./gms.scm --recycle-removed)

Then use [jSite](https://github.com/Bombe/jSite) or `freesitemgr` from [pyFreenet](https://github.com/freenet/pyFreenet) to upload the site/ folder into Freenet.

Insert on debian (example)

    sudo apt install git python3 autoconf automake openjdk-17-jdk wget grep sed gnupg curl firefox-esr
    pip3 install --user pyFreenet3
    git clone https://github.com/freenet/browser
    (cd browser && ./bootstrap.sh --prefix=$HOME/.local && make install)
    # install Freenet if needed
    ~/.local/bin/freenetbrowser --install
    # add guile-media-site/site
    ~/.local/bin/freesitemgr add
    # get your site public key (the website address to give others)
    ~/.local/bin/freesitemgr list YOUR_SITENAME

