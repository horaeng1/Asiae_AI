class Person:
    def __init__(self, name="Guest"):
        self.__name=name
    def setname(self, name): #setter : 내용수집
        self.__name=name
    def getname(self): #getter : 내용출력
        return self.__name
    #클래스 사용시 setter와 getter는 매번 만들어줘야함
