#push to git
system("pwd")
system("git init")
system("git remote add origin 'https://github.com/cerees/everify_diffusion'")
system("git add .")
system("git commit -m 'initial commit'")
#system("git pull master origin")
system("git push")