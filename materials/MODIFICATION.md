<div><ol>
<li>W swoim programie stwórz task "Inspector", posiadający entry Start (tak, jak Consumer i Producer)</li>
<li>W "ciele" tasku symuluj wykonywanie inspektora z pewnym stałym czasem trwania i wywołuj task bufora&nbsp;Inspection_In_Storage&nbsp;</li>
<li>W tasku Bufora dodaj entry Inspection_In_Storage, a w jego ciele:<ol>
<li>procedurę/funkcję Inspection_process, która zabiera z magazynu cały zasób tego produktu, którego liczba w magazynie przekroczyła pewien próg n; np.</li>
<li>
<pre>Storage contents: 1 Product1<br>Storage contents: 4 Product2<br>Storage contents: 1 Product3<br>Storage contents: 11 Product4<br>Storage contents: 7 Product5<br>INSPECTION START: You have more than 10 products Product4 in your storage - an inspection has taken place<br>...<br>INSPECTION RESULT: You have stored too many products Product4 you need to throw them away and clean up<br>Storage contents: 1 Product1<br>Storage contents: 4 Product2<br>Storage contents: 1 Product3<br>Storage contents: 0 Product4<br>Storage contents: 7 Product5</pre>
</li>
<li>jeśli nie użyłeś select w buforze to go dodaj</li>
<li>do selecta dodaj jeszcze jedno "or" z akceptacją&nbsp;Inspection_In_Storage i wywołaniem&nbsp;Inspection_process</li>
</ol></li>
<li>Pamiętaj o starcie tasku "Inspection_In_Storage" na końcu programu, w tym bloku:</li>
</ol>
<pre>begin<br>for I in 1 .. Number_Of_Products loop<br>P(I).Start(I, 10);<br>end loop;<br>for J in 1 .. Number_Of_Consumers loop<br>K(J).Start(J,12);<br>end loop;<br>end Simulation;</pre>
<p><span>Umieść rozwiązanie (.adb) na moodlu (nie pakuj plików)</span></p>
<p><span>Na rozwiązanie i wgranie zadania masz 1h</span></p>
<p>Powodzenia!</p></div>
