init:
        git worktree add -B gh-pages publish origin/gh-pages
        npm install
build file_prefix=`find content -name '*.typ' | sed 's/\.typ$//' | fzf`:
        typst compile --format html --features html {{file_prefix}}.typ
        mv {{file_prefix}}.html ./public/posts

serve:
        devd -ol ./public
deploy: 
        #!/usr/bin/env bash
        cp -r ./public/* ./publish/
        cd ./publish/
        git add .
        git commit -m "Update site"
        git push
