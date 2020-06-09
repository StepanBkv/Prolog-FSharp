open System
open System.Drawing
open System.IO
open System.Windows.Forms

type solutio = 
    None
    | One of float
    | Two of float*float

let solution (a:float) (b:float) (c:float) = 
    let (diskr:float) = b*b - ((4.0) * a * c)
    if diskr < 0.0 then None
    else if diskr = 0.0 then One(-b / 2.0 * a)
    else Two((-b - Math.Sqrt(diskr))/ (2.0 * a),(-b + Math.Sqrt(diskr))/ (2.0 * a))
                             
let rec readlist b list = function
    "" when (b = "") -> list
    | "" when (b <> "") -> Convert.ToInt32(b)::list
    | n when (n.[0] = ' ') -> readlist "" (Convert.ToInt32(b)::list) (n.Remove(0,1))
    | s when (s.[0] <> ' ')-> readlist (b + (s.[0].ToString())) list (s.Remove(0,1))

let form = new Form(Width= 236, Height = 289, Text = "Главная форма")
let button = new Button(Text = "Нажмите", Top = 100)

form.Controls.Add(button)

let RB = new RichTextBox(Width = 120, Height = 30,Font = new Font(FontFamily("Consolas"), 16.0f,FontStyle.Bold), Top = 40)
let RB2 = new RichTextBox(Width = 220, Height = 70, Font = new Font(FontFamily("Consolas"), 10.0f,FontStyle.Bold), Top = 180)
let inp = new Label(Text = "Input:", Top = 5, ForeColor = Color.Red, Font = new Font(FontFamily("Times New Roman"), 15.0f, FontStyle.Regular))
let outp = new Label(Text = "Output:", Top = 150, ForeColor = Color.Red, Font = new Font(FontFamily("Times New Roman"), 15.0f, FontStyle.Regular))

form.Controls.Add(RB)
form.Controls.Add(outp)
form.Controls.Add(inp)
form.Controls.Add(RB2)

let equation = 
    button.MouseClick
    |> Observable.map(fun ClickArgs-> let str = RB.Text
                                      RB2.Clear()
                                      let (koeff_list:int list) = readlist "" [] str
                                      let result = solution ((float)(List.item 2 koeff_list)) ((float)(List.item 1 koeff_list)) ((float)(List.item 0 koeff_list))
                                      match result with
                                        None -> RB2.AppendText("Корни уравнения - комплексные")
                                        |One(x) -> RB2.AppendText("Решением является:" + x.ToString())
                                        |Two(x,y) -> RB2.AppendText("Решением является\nx1 = " + x.ToString() + "\n" + "x2 = " + y.ToString())
                                      ())
equation
|>Observable.add(fun x -> ())

do Application.Run(form)
                                      
