require 'chronic'

class MainController < ApplicationController
  before_filter :authenticate, :only => :ssl

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
  
  def ssl
    render :text => request.ssl?.to_s
  end
  
  def ip
    render :text => request.remote_ip.to_s
  end
  
  protected

  def authenticate
    authenticate_or_request_with_http_basic do |username, password|
      username == "foo" && password == "bar"
    end
  end
end
