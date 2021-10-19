#1. Merging when there are common fields with the same name
data0 = read.table(header = TRUE, text = '
                  subjectid storyid rating
                  1         1       4
                  1         2       5
                  2         3       4
                  3         4       3
                  3         5       2
                  ')
data0
stories0 = read.table(header = TRUE, text = '
                  title   storyid subject
                  ABC     1       Alpha
                  XYZ     2       Gamma
                  MEO     3       Cats
                  LAL     4       Colours
                  KIK     5       Actions
                  ')
stories0
merge(stories0, data0, "storyid")
#------------------------------------------------
#2. Merging when there are common fields with the same name
data = read.table(header = TRUE, text = '
                  subjectid storyid rating
                  1         1       4
                  1         2       5
                  2         3       4
                  3         4       3
                  3         5       2
                  ')
data
stories = read.table(header = TRUE, text = '
                  title   id      subject
                  ABC     1       Alpha
                  XYZ     2       Gamma
                  MEO     3       Cats
                  LAL     4       Colours
                  KIK     5       Actions
                  ')
stories
#The column name defined in x will be used
merge(x = stories, y = data, by.x = "id", by.y = "storyid")
merge(y = stories, x = data, by.y = "id", by.x = "storyid")
#------------------------------------------------
#3. Merging by multiple columns
animals = read.table(header = TRUE, text = '
                     size   type  name
                     
                     small  cat   lynx
                     big    cat   tiger
                     small  dog   chihuahua
                     big    dog   "great dane"
                     ')
animals
observations = read.table(header = TRUE, text = '
                          number  size  type
                          1       big   cat
                          2       small dog
                          3       small dog
                          4       big   dog
                          ')
observations
merge(animals, observations, c("type", "size"))