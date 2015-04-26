public class sample{ 
		public interface Lambda{ 
			public Lambda app(Lambda arg);
			public String toString();		
		}
public static class Lambda1  implements Lambda {
 protected String str;
 protected String buff;
public Lambda1(String s,String b){
str = s;
buff = b;
		}
public Lambda app(Lambda arg){ 
				return arg;
			}
public String toString() {
if(buff == null)
			    {
			       return str;
			    }
			    else
			    {
				return str+buff;
			    }
               	}	
		}
public static class Lambda2  implements Lambda {
 protected String str;
 protected String buff;
public Lambda2(String s,String b){
str = s;
buff = b;
		}
public Lambda app(Lambda arg){ 
				return arg;
			}
public String toString() {
if(buff == null)
			    {
			       return str;
			    }
			    else
			    {
				return str+buff;
			    }
               	}	
		}

public static void main(String[] args)
			{ 
				Lambda1 l1 = new Lambda1("s",null);
				Lambda2 l2 = new Lambda2("z","sz" );
		}
	}