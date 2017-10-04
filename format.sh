for FILE in *.lhs; do
    FILE="${FILE%%.*}"
    pandoc $FILE.lhs --standalone --self-contained -o $FILE.html
    replace="<title>Building a CPU :: Will Yager</title> <link href='http://fonts.googleapis.com/css?family=Lato' rel='stylesheet' type='text/css'> <link rel=\"icon\" type=\"image/png\" href=\"https://yager.io/yager.png\"/><link href='/post.css' rel='stylesheet' type=\"text/css\">"
    replaceEscaped=$(sed 's/[&/\]/\\&/g' <<<"$replace")
    sed -i -e "s/<title><\/title>/$replaceEscaped/g" $FILE.html
done
