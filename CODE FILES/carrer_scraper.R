library(XML)
career_scraper=function(id,name)
{
  player="http://stats.espncricinfo.com/ci/engine/player/"
  bat=".html?class=2;template=results;type=batting;view=innings"
  bowl=".html?class=2;template=results;type=bowling;view=innings"
  result=".html?class=2;template=results;type=allround;view=results"
  
  url_bat=paste0(player,id,bat)
  url_bowl=paste0(player,id,bowl)
  url_result=paste0(player,id,result)
  
  table_bat=readHTMLTable(url_bat)
  table_bowl=readHTMLTable(url_bowl)
  table_result=readHTMLTable(url_result)
  
  bat=table_bat$"Innings by innings list"
  bowl=table_bowl$"Innings by innings list"
  result=table_result$"Match results"$"Result"
  batting_records=cbind(bat,result)
  balling_records=cbind(bowl,result)
  
  csvfile="C:\\Users\\SIDDHESH\\Desktop\\Data Science - Aegis\\Capstone project\\Code\\Team Code\\PROJECT\\PROJECT\\DATA\\Career Data\\"
  bat_csv=paste0(csvfile,name,"_career_bat.csv")
  bowl_csv=paste0(csvfile,name,"_career_bowl.csv")
  
  write.csv(batting_records,bat_csv)
  write.csv(balling_records,bowl_csv) 
}
career_scraper("253802","virat")