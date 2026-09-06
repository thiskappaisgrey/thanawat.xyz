init:
        git worktree add -B gh-pages publish origin/gh-pages
        npm install

spellcheck file_prefix:
        #!/usr/bin/env bash
        words=$(aspell --personal=./.aspell.en.pws --lang=en list < {{file_prefix}}.typ | sort -u)
        if [ -n "$words" ]; then
                echo "Possible misspellings in {{file_prefix}}.typ:"
                echo "$words" | sed 's/^/  /'
        fi

spellcheck-all:
        #!/usr/bin/env bash
        for f in content/*.typ; do
                just spellcheck "${f%.typ}"
        done

build file_prefix=`find content -name '*.typ' | sed 's/\.typ$//' | fzf`:
        just spellcheck {{file_prefix}}
        typst compile --format html --features html {{file_prefix}}.typ
        mv {{file_prefix}}.html ./public/posts

watch file_prefix=`find content -name '*.typ' | sed 's/\.typ$//' | fzf`:
        watchexec -e typ -w {{file_prefix}}.typ -- just build {{file_prefix}}

serve:
        devd -ol ./public
publish: deploy
deploy: 
        #!/usr/bin/env bash
        cp -r ./public/* ./publish/
        cd ./publish/
        git add .
        git commit -m "Update site"
        git push
