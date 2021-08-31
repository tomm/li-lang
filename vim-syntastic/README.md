Find your vim syntastic plugin directory.
If you are using pathogen, this will be:
    $HOME/.vim/bundle/syntastic/

Then run:
    mkdir syntax_checkers/li

Copy li.vim into this directory.

In the syntastic plugin directory, edit plugin/syntastic/registry.vim
Add an entry to s:_DEFAULT_CHECKERS:
        \ 'li':            ['lic'],

That should be enough to enable li syntastic checking.
