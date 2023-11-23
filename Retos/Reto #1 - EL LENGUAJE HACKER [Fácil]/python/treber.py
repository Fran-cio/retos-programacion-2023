"""
Escribe un programa que reciba un texto y transforme lenguaje natural a
 * "lenguaje hacker" (conocido realmente como "leet" o "1337"). Este lenguaje
 *  se caracteriza por sustituir caracteres alfanuméricos.
 * - Utiliza esta tabla (https://www.gamehouse.com/blog/leet-speak-cheat-sheet/) 
 *   con el alfabeto y los números en "leet".
 *   (Usa la primera opción de cada transformación. Por ejemplo "4" para la "a")

 """

leetAlphabet = {'a':'4','b':'I3','c':'[','d':')','e':'3','f':'|=','g':'&','h':'#','i':'1','j':',_|',
                'k':'>|','l':'1','m':'/\/\'','n':'^/','o':'0','p':'|*','q':'(_,)','r':'I2',
                's':'5','t':'7','u':'(_)','v':'\/','w':'\/\/','x':'><','y':'j','z':'2'} 

leetNumber = {"0":"o", "1":"L", "2":"R", "3":"E", "4":"A", "5":"S", "6":"b", "7":"T", "8":"B", "9":"g"}


def inputTxt():

    text = input("Escribe su texto aquí (solo alfanumerico): ").lower()
    if text.isalnum():
        return text
    else:
        return inputTxt()


def hackerTxt(text):
    
    txtTransform = ""

    for caracter in text:
        if caracter in leetAlphabet.keys():
            txtTransform+=  leetAlphabet[caracter]
        else:
            txtTransform+= leetNumber[caracter]

    return txtTransform


text = inputTxt()
newTxt = hackerTxt(text)
print(newTxt)
