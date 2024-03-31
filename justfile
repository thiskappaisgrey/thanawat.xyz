init:
        git worktree add -B gh-pages publish origin/gh-pages
        npm install
build:
        npx tailwindcss -i index.css -o static/index.css
        zola build

serve:
        npx tailwindcss -i index.css -o static/index.css &> tailwind.out & echo $! > tailwindpid
        zola serve &> zola.out & echo $! > zolapid
kill:
        #!/usr/bin/env bash
        kill -9 $(cat tailwindpid)
        kill -9 $(cat zolapid)

deploy: (build)
        cp -r ./public/* ./publish/
        cd ./publish/
        git add .
        git commit -m "Update site"
