# trafooft: These R a few of our favourite things

So Maria is chatting about Gertl's analysis over coffee and Maria says "I've
got a script that will help with that...". And Gertl says "But Maria, I am
surely not the only person who can benefit from your wisdom". And Maria says
"You're right". So she clicks on Create New File and puts her script here, with
a little note saying what it does.

## How to use

`trafooft` is a happy dumping ground for R code. You can add R scripts or
RMarkdown notebooks to your heart's content. You can organize your files
however you like, but one easy default option is to put the R scripts or
notebooks at the top level, and put your data files inside the general purpose
`data/` directory. In your scripts and notebooks, you should then refer to the data files with e.g. 

```r
read_csv('data/foobar.csv')
```

## All code here is open source

Obviously, all code here is free and open source. But it ought to have an
explicit licence nonetheless. That licence is in `license.txt`, and is God's
favourite open source licence, i.e. GPL. 

## Open data too

Proprietary data is evil. It belongs in the hottest parts of hell. It does not
belong here. By putting your data files here, they magically become open data
by virtue of the open data licence part of `license.txt`.
