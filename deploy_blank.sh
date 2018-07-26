#!/bin/bash
# deploy site to VPS 
# generated by Andrey Prokopenko
# params
HOST=""
BLOG_REMOTE=""
USERNAME=""
BLOG_PATH=""
# prepare zip
ghc site.hs && ./site rebuild
rm -f blog.zip
zip -r blog.zip _site
# upload zip on host
scp blog.zip ${USERNAME}@$HOST:"$BLOG_PATH/blog.zip"
# extract fresh blog
ssh ${USERNAME}@${HOST} "cd $BLOG_REMOTE && find . -type f -not -name 'blog.zip' | xargs rm -rf && unzip -o blog.zip && rm blog.zip"
# setting permissions
ssh ${USERNAME}@$HOST "chown -R www-data ${BLOG_REMOTE}/_site"
