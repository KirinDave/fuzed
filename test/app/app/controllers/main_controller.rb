require 'chronic'

class MainController < ApplicationController
  def index
    
  end
  
  def say
    render :text => "hello world - #{session[:time]}"
  end
  
  def set
    session[:time] = Time.now.to_s
    render :text => 'done'
  end
  
  def chronic
    s = Chronic.parse(params[:id])
    render :text => s ? s.strftime('%d %b %Y @ %I:%M.%S %p') : '...'
  end
  
  def ready
    
  end
  
  def go
    render :text => 'done'
  end
  
  def hog
    loop { }
  end
  
end
