# function to obtain legend_name according to keywords
legendName=function(keywords){
    if(length(keywords)==1) {
        legend_name=keywords[1]
    } else if (length(keywords)==2) {
        legend_name=paste(keywords[1],', ',keywords[2],sep='')
    } else {
        legend_name=paste(keywords[1],', ',keywords[2],', ...',sep='')
    }
    return(legend_name)
}
# one group of keywords plot function
trendPlot=function(covid,keywords,trend){
    # select covid trend data
    trend=trend%>%mutate(date=ymd(date))
    date=trend$date%>%{ymd('1970-01-01')+max(.):min(.)}
    covid=covid%>%mutate(date=ymd(date))
    covid=data.frame(date)%>%
        left_join(covid,'date')
    trend=data.frame(date)%>%
        left_join(trend,'date')
    trend$number[is.na(trend$number)]=0
    trend$sentiment_score[is.na(trend$sentiment_score)]=0
    # plot for daily covid trend
    hover=paste('Date: ',covid[,1],' <br>Daily Increase: ',covid[,2])
    pic=plot_ly(x=covid[,1],y=covid[,2],color=I('black'),text=hover,
                hoverinfo='text',name='Daily case increase',type ='scatter',
                mode='lines+markers')
    # plot for trend
    ## trend legend plot
    if(length(keywords)==1) {
        legend_name=keywords[1]
    } else if (length(keywords)==2) {
        legend_name=paste(keywords[1],', ',keywords[2],sep='')
    } else {
        legend_name=paste(keywords[1],', ',keywords[2],', ...',sep='')
    }
    pic=pic%>%
        add_trace(pic,x=trend$date,y=trend$number,color=I('blue'),
                  name=legend_name,mode='lines+markers',yaxis="y2",
                  marker=list(symbol=2,size=10),visible='legendonly')
    ## plot for sentiment legend
    pic=pic%>%
        add_trace(x=trend$date[1],y=trend$number[1],color=I('green'),name="Possitive",
                  mode='markers',marker=list(size=15),yaxis="y2",visible='legendonly')
    pic=pic%>%
        add_trace(x=trend$date[1],y=trend$number[1],color=I('red'),name="Negative",
                  mode='markers',marker=list(size=15),yaxis="y2",visible='legendonly')
    ## trend sentiment & frequency plot
    n=nrow(trend)
    color=trend$sentiment_score%>%
        {(.[1:(n-1)]+.[2:n])/2}%>%
        {ifelse(.>0,'green','red')}
    hover=paste('Date: ',trend$date,' <br>Sentiment Score: ',round(trend$sentiment_score,3))
    for(i in 1:(n-1)){
        pic=pic%>%add_trace(x=trend$date[i:(i+1)],y=trend$number[i:(i+1)],color=I(color[i]),
                            text=hover[i],hoverinfo='text',marker=list(symbol=2,size=10),
                            mode='lines+markers',yaxis="y2",showlegend=F)
    }
    # set entire layout
    pic%>%layout(title="Trends between the COVID-19 and sentiment",
                 yaxis=list(title="Number of infections on each day"),
                 yaxis2=list(tickfont=list(color="blue"),overlaying="y",
                             side="right",title="The frequency of keywords"),
                 xaxis=list(title="Date"))
}

# two groups of keywords plot function
trendsPlot=function(covid,keywords,trends){
    # select covid trend data
    trends[[1]]=trends[[1]]%>%mutate(date=ymd(date))
    trends[[2]]=trends[[2]]%>%mutate(date=ymd(date))
    date=c(trends[[1]]$date,trends[[2]]$date)%>%{ymd('1970-01-01')+max(.):min(.)}
    covid=covid%>%mutate(date=ymd(date))
    covid=data.frame(date)%>%
        left_join(covid,'date')
    trends[[1]]=data.frame(date)%>%
        left_join(trends[[1]],'date')
    trends[[2]]=data.frame(date)%>%
        left_join(trends[[2]],'date')
    trends[[1]]$number[is.na(trends[[1]]$number)]=0
    trends[[2]]$number[is.na(trends[[2]]$number)]=0
    trends[[1]]$sentiment_score[is.na(trends[[1]]$sentiment_score)]=0
    trends[[2]]$sentiment_score[is.na(trends[[2]]$sentiment_score)]=0
    # plot for daily trend
    hover=paste('Date: ',covid[,1],' <br>Daily Increase: ',covid[,2])
    pic=plot_ly(x=covid[,1],y=covid[,2],color=I('black'),text=hover,
                hoverinfo='text',name='Daily case increase',type ='scatter',
                mode='lines+markers')
    # plot for legend
    ## trend1 legend plot
    if(length(keywords[[1]])==1) {
        legend_name=keywords[[1]][1]
    } else if (length(keywords[[1]])==2) {
        legend_name=paste(keywords[[1]][1],', ',keywords[[1]][2],sep='')
    } else {
        legend_name=paste(keywords[[1]][1],', ',keywords[[1]][2],', ...',sep='')
    }
    pic=pic%>%add_trace(x=trends[[1]]$date,y=trends[[1]]$number,color=I('blue'),
                        name=legend_name,mode='lines+markers',yaxis="y2",
                        marker=list(symbol=2,size=10),visible='legendonly')
    ## trend2 legend plot
    if(length(keywords[[2]])==1) {
        legend_name=keywords[[2]][1]
    } else if (length(keywords[[2]])==2) {
        legend_name=paste(keywords[[2]][1],', ',keywords[[2]][2],sep='')
    } else {
        legend_name=paste(keywords[[2]][1],', ',keywords[[2]][2],', ...',sep='')
    }
    pic=pic%>%add_trace(x=trends[[2]]$date,y=trends[[2]]$number,color=I('blue'),
                        name=legend_name,mode='lines+markers',yaxis="y2",
                        marker=list(symbol=8,size=10),visible='legendonly')
    ## plot for sentiment legend
    pic=pic%>%
        add_trace(x=trends[[1]]$date[1],y=trends[[1]]$number[1],color=I('green'),name="Possitive",mode='markers',
                  marker=list(size=15), yaxis="y2",visible='legendonly')
    pic=pic%>%
        add_trace(x=trends[[1]]$date[1],y=trends[[1]]$number[1],color=I('red'),name="Negative",mode='markers',
                  marker=list(size=15), yaxis="y2",visible='legendonly')
    # plot for trend1
    n=nrow(trends[[1]])
    color=trends[[1]]$sentiment_score%>%
        {(.[1:(n-1)]+.[2:n])/2}%>%
        {ifelse(.>0,'green','red')}
    hover=paste('Date: ',trends[[1]]$date,' <br>Sentiment Score: ',round(trends[[1]]$sentiment_score,3))
    for(i in 1:(n-1)){
        pic=pic%>%add_trace(x=trends[[1]]$date[i:(i+1)],y=trends[[1]]$number[i:(i+1)],color=I(color[i]),
                            text=hover[i],hoverinfo='text',marker=list(symbol=2,size=10),
                            mode='lines+markers',yaxis="y2",showlegend=F)
    }
    # plot for trend2
    n=nrow(trends[[2]])
    color=trends[[2]]$sentiment_score%>%
        {(.[1:(n-1)]+.[2:n])/2}%>%
        {ifelse(.>0,'green','red')}
    hover=paste('Date: ',trends[[2]]$date,' <br>Sentiment Score: ',round(trends[[2]]$sentiment_score,3))
    for(i in 1:(n-1)){
        pic=pic%>%add_trace(x=trends[[2]]$date[i:(i+1)],y=trends[[2]]$number[i:(i+1)],color=I(color[i]),
                            text=hover[i],hoverinfo='text',marker=list(symbol=8,size=10),
                            mode='lines+markers',yaxis="y2",showlegend=F)
    }
    # set entire layout
    pic%>%layout(title="Trends between the COVID-19 and Twitter sentiment",
                 yaxis=list(title="Number of infections on each day"),
                 yaxis2=list(tickfont=list(color="blue"),overlaying="y",
                             side="right",title="The frequency of keywords"),
                 xaxis=list(title="Date"))
}

# geo map function
geoTrendMap=function(covid,trend){
    # merge data
    data=left_join(trend,covid,c('state','month'))%>%
        select(month,number,sentiment_score,state,positiveIncrease)
    data$number[is.na(data$number)]=0
    data$sentiment_score[is.na(data$sentiment_score)]=0
    data=mutate(data,hover=with(data,paste(state,"<br> <br> Positive:",positiveIncrease,
                                           "<br> Number of Tweets",number,
                                           "<br> Sentiment Score",round(sentiment_score,3))))
    # background map
    pic=plot_geo(locationmode='USA-states')
    # monthly maps
    n=data$month%>%unique()%>%length()
    visible=c(T,rep(F,n-1),T,T)
    steps=list()
    for (i in 1:n) {
        pic=data[data$month==unique(data$month)[i],]%>%
            {add_trace(pic,locations=.$state,z=.$sentiment_score,text=.$hover,
                       hoverinfo='text',visible=visible[i],type='choropleth',colors="RdBu")}
        steps[[i]]=list(args=list('visible',c(rep(F,i-1),T,rep(F,n-i),T,T)),
                                  label=month(unique(data$month)[i],T),method='restyle')
    }
    # add slider control & modify entire layout
    pic%>%
        add_trace(x=0,y=0,color=I('blue'),name="Positive",mode='markers',
                  marker=list(size=15),visible='legendonly')%>%
        add_trace(x=0,y=0,color=I('red'),name="Negative",mode='markers',
                  marker=list(size=15),visible='legendonly')%>%
        layout(title="Sentiment Score of States",
               geo=list(scope='usa',projection=list(type='albers usa'),
                        showlakes=T,lakecolor=toRGB('white')),
               sliders=list(list(active=1,currentvalue=list(prefix="Month: "),
                                 steps=steps)))%>%
        hide_colorbar()
}

# geo trend function
geoTrendPlot=function(covid,keywords,trend){
    # select covid trend data
    covid=trend$month%>%
        {data.frame(month=min(.):max(.))}%>%
        left_join(covid,'month')
    covid$month=month(covid$month)
    # plot for daily covid trend
    hover=paste('Month: ',covid[,1],' <br>Monthly Increase: ',covid[,2])
    pic=plot_ly(x=covid[,1],y=covid[,2],color=I('black'),text=hover,
                hoverinfo='text',name='Monthly case increase',type ='scatter',
                mode='lines+markers')
    # plot for trend
    ## trend legend plot
    if(length(keywords)==1){
        legend_name=keywords[1]
    }
    else{
        if(length(keywords)==2){
            legend_name=paste(keywords[1],', ',keywords[2],sep='')
        }
        else {
            legend_name=paste(keywords[1],', ',keywords[2],', ...',sep='')
        }
    }
    pic=pic%>%
        add_trace(x=trend$month,y=trend$number,color=I('blue'),
                  name=legend_name,mode='lines+markers',yaxis="y2",
                  marker=list(symbol=2,size=10),visible='legendonly')
    ## plot for sentiment legend
    pic=pic%>%
        add_trace(x=trend$month[1],y=trend$number[1],color=I('green'),name="Possitive",
                  mode='markers',marker=list(size=15),yaxis="y2",visible='legendonly')
    pic=pic%>%
        add_trace(x=trend$month[1],y=trend$number[1],color=I('red'),name="Negative",
                  mode='markers',marker=list(size=15),yaxis="y2",visible='legendonly')
    ## trend sentiment & frequency plot
    n=nrow(trend)
    color=trend$sentiment_score%>%
        {(.[1:(n-1)]+.[2:n])/2}%>%
        {ifelse(.>0,'green','red')}
    hover=paste('Month: ',trend$month,' <br>Sentiment Score: ',round(trend$sentiment_score,3))
    for(i in 1:(n-1)){
        pic=pic%>%add_trace(x=trend$month[i:(i+1)],y=trend$number[i:(i+1)],color=I(color[i]),
                            text=hover[i],hoverinfo='text',marker=list(symbol=2,size=10),
                            mode='lines+markers',yaxis="y2",showlegend=F)
    }
    # set entire layout
    pic%>%layout(title="Trends between the COVID-19 and sentiment",
                 yaxis=list(title="Number of infections in each month"),
                 yaxis2=list(tickfont=list(color="blue"),overlaying="y",
                             side="right",title="The frequency of keywords"),
                 xaxis=list(title="Month"))

}