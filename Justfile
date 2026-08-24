init:
        git worktree add -B gh-pages publish origin/gh-pages
        npm install
build:
        echo "no build step"

serve:
        devd -ol ./public
deploy: (build)
        #!/usr/bin/env bash
        cp -r ./public/* ./publish/
        cd ./publish/
        git add .
        git commit -m "Update site"
        git push
